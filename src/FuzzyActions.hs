{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module FuzzyActions where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (QueryPoint (QueryTip), localNodeConnectInfo, queryUTxOFor)
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception (ErrorCall (..))
import Control.Lens ((^?))
import Data.Aeson (Value, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _JSON, _String)
import Data.Char (toLower)
import Data.List (isInfixOf, (!!))
import Data.Map.Strict qualified as Map
import FuzzyReport
import Hydra.Cardano.Api (
  NetworkId (..),
  NetworkMagic (..),
  PaymentKey,
  SigningKey,
  SocketPath,
  TxIn,
  TxOut,
  UTxO,
  VerificationKey,
  getTxBody,
  getTxId,
  lovelaceToValue,
  mkVkAddress,
  selectLovelace,
  signTx,
  txOutAddress,
  txOutValue,
 )
import Hydra.Cardano.Api qualified as CAPI
import Hydra.Chain.Backend (submitTransaction)
import Hydra.Chain.Direct (runDirectBackend)
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Util (keysFor)
import Hydra.Ledger.Cardano (mkSimpleTx)
import Hydra.Logging (nullTracer)
import Hydra.Network (Host (..))
import Hydra.Options (DirectOptions (..), defaultDirectOptions)
import Hydra.Tx.Secret (Secret, withSecret)
import HydraNode (
  HydraClient,
  getSnapshotUTxO,
  input,
  postDecommit,
  requestCommitTx,
  send,
  waitMatch,
  withConnectionToNodeHost,
 )
import Network.HTTP.Conduit (parseUrlThrow)
import Network.HTTP.Simple (httpLbs)
import System.Random (StdGen, mkStdGen, randomR)

-- * Types

demoNetworkId :: NetworkId
demoNetworkId = Testnet (NetworkMagic 42)

data FuzzyOptions = FuzzyOptions
  { fuzzyRounds :: Int
  , fuzzyTxsPerRound :: Int
  , fuzzyStress :: Bool
  , fuzzyParallelism :: Int
  , fuzzyStuckTimeout :: NominalDiffTime
  , fuzzySeed :: Int
  , fuzzyOutputDir :: FilePath
  , fuzzyAliceHost :: Host
  , fuzzyBobHost :: Host
  , fuzzyCarolHost :: Host
  , fuzzySocketPath :: FilePath
  , fuzzyDepositExpireWait :: NominalDiffTime
  }

defaultFuzzyOptions :: FuzzyOptions
defaultFuzzyOptions =
  FuzzyOptions
    { fuzzyRounds = 3
    , fuzzyTxsPerRound = 50
    , fuzzyStress = False
    , fuzzyParallelism = 3
    , fuzzyStuckTimeout = 30
    , fuzzySeed = 42
    , fuzzyOutputDir = "."
    , fuzzyAliceHost = Host "127.0.0.1" 4001
    , fuzzyBobHost = Host "127.0.0.1" 4002
    , fuzzyCarolHost = Host "127.0.0.1" 4003
    , fuzzySocketPath = "devnet/node.socket"
    , fuzzyDepositExpireWait = 30
    }

data FuzzyParty = FuzzyAlice | FuzzyBob | FuzzyCarol
  deriving stock (Show, Eq, Ord, Enum, Bounded)

partyId :: FuzzyParty -> Int
partyId = \case
  FuzzyAlice -> 1
  FuzzyBob -> 2
  FuzzyCarol -> 3

data FuzzyEnv = FuzzyEnv
  { envAliceClient :: HydraClient
  , envBobClient :: HydraClient
  , envCarolClient :: HydraClient
  , envAliceFundsSk :: Secret (SigningKey PaymentKey)
  , envBobFundsSk :: Secret (SigningKey PaymentKey)
  , envCarolFundsSk :: Secret (SigningKey PaymentKey)
  , envAliceFundsVk :: VerificationKey PaymentKey
  , envBobFundsVk :: VerificationKey PaymentKey
  , envCarolFundsVk :: VerificationKey PaymentKey
  , envDirectOpts :: DirectOptions
  }

clientFor :: FuzzyEnv -> FuzzyParty -> HydraClient
clientFor env = \case
  FuzzyAlice -> envAliceClient env
  FuzzyBob -> envBobClient env
  FuzzyCarol -> envCarolClient env

fundsSkFor :: FuzzyEnv -> FuzzyParty -> Secret (SigningKey PaymentKey)
fundsSkFor env = \case
  FuzzyAlice -> envAliceFundsSk env
  FuzzyBob -> envBobFundsSk env
  FuzzyCarol -> envCarolFundsSk env

fundsVkFor :: FuzzyEnv -> FuzzyParty -> VerificationKey PaymentKey
fundsVkFor env = \case
  FuzzyAlice -> envAliceFundsVk env
  FuzzyBob -> envBobFundsVk env
  FuzzyCarol -> envCarolFundsVk env

-- * Entry point

runFuzzy :: FuzzyOptions -> IO ()
runFuzzy opts = do
  (aliceFundsVk, aliceFundsSk) <- keysFor AliceFunds
  (bobFundsVk, bobFundsSk) <- keysFor BobFunds
  (carolFundsVk, carolFundsSk) <- keysFor CarolFunds

  let socketPath :: SocketPath = fromString (fuzzySocketPath opts)
  let directOpts = defaultDirectOptions{nodeSocket = socketPath, networkId = demoNetworkId}
  let mkEnv aliceClient bobClient carolClient =
        FuzzyEnv
          { envAliceClient = aliceClient
          , envBobClient = bobClient
          , envCarolClient = carolClient
          , envAliceFundsSk = aliceFundsSk
          , envBobFundsSk = bobFundsSk
          , envCarolFundsSk = carolFundsSk
          , envAliceFundsVk = aliceFundsVk
          , envBobFundsVk = bobFundsVk
          , envCarolFundsVk = carolFundsVk
          , envDirectOpts = directOpts
          }
  let cfg =
        RunConfig
          { cfgRounds = fuzzyRounds opts
          , cfgTxsPerRound = fuzzyTxsPerRound opts
          , cfgSeed = fuzzySeed opts
          , cfgStuckTimeout = fuzzyStuckTimeout opts
          }
  rngRef <- newIORef (mkStdGen (fuzzySeed opts))
  allStats <- forM [1 .. fuzzyRounds opts] $ \n ->
    withConnectionToNodeHost nullTracer 1 (fuzzyAliceHost opts) Nothing (Just "/?history=no") $ \aliceClient ->
      withConnectionToNodeHost nullTracer 2 (fuzzyBobHost opts) Nothing (Just "/?history=no") $ \bobClient ->
        withConnectionToNodeHost nullTracer 3 (fuzzyCarolHost opts) Nothing (Just "/?history=no") $ \carolClient ->
          runRound (mkEnv aliceClient bobClient carolClient) opts n rngRef

  reportPath <- writeRunReport (fuzzyOutputDir opts) cfg allStats
  putStrLn $ "\nRun report written to: " <> reportPath
  let bugs = length $ filter (isJust . roundFailure) allStats
  when (bugs > 0) $
    putStrLn $
      "BUGS FOUND: " <> show bugs

-- * Round execution

data RoundCtx = RoundCtx
  { ctxStatsRef :: IORef RoundStats
  , ctxActionLogRef :: IORef [ActionEntry]
  , ctxServerLogRef :: IORef [Value]
  , ctxLastSnapshotRef :: IORef UTCTime
  , ctxPendingTxRef :: IORef Int
  , ctxRngRef :: IORef StdGen
  }

newRoundCtx :: Int -> UTCTime -> IORef StdGen -> IO RoundCtx
newRoundCtx n now rngRef = do
  ctxStatsRef <- newIORef (emptyRoundStats n now)
  ctxActionLogRef <- newIORef []
  ctxServerLogRef <- newIORef []
  ctxLastSnapshotRef <- newIORef now
  ctxPendingTxRef <- newIORef 0
  pure
    RoundCtx
      { ctxStatsRef
      , ctxActionLogRef
      , ctxServerLogRef
      , ctxLastSnapshotRef
      , ctxPendingTxRef
      , ctxRngRef = rngRef
      }

getHeadStatus :: HydraClient -> IO Text
getHeadStatus client =
  waitMatch 15 client $ \v ->
    v
      ^? key "tag" . _String >>= \case
        "Greetings" -> v ^? key "headStatus" . _String
        _ -> Nothing

runRound :: FuzzyEnv -> FuzzyOptions -> Int -> IORef StdGen -> IO RoundStats
runRound env opts roundNum rngRef = do
  putStrLn $ "\n=== Round " <> show roundNum <> " of " <> show (fuzzyRounds opts) <> " ==="
  now <- getCurrentTime
  ctx <- newRoundCtx roundNum now rngRef

  headStatus <- getHeadStatus (envAliceClient env)
  putStrLn $ "Head status: " <> toString headStatus

  let phases = case headStatus of
        "Idle" ->
          initPhase env ctx
            >> openPhase env opts ctx
            >>= \snapshotUtxo -> closePhase env opts ctx >> fanoutPhase env snapshotUtxo ctx
        "Open" ->
          openPhase env opts ctx
            >>= \snapshotUtxo -> closePhase env opts ctx >> fanoutPhase env snapshotUtxo ctx
        "Initializing" ->
          waitForOpen (envAliceClient env) ctx
            >> openPhase env opts ctx
            >>= \snapshotUtxo -> closePhase env opts ctx >> fanoutPhase env snapshotUtxo ctx
        "Closed" -> do
          snapshotUtxo <- getSnapshotUTxO (envAliceClient env)
          closePhase env opts ctx >> fanoutPhase env snapshotUtxo ctx
        "FanoutPossible" -> do
          snapshotUtxo <- getSnapshotUTxO (envAliceClient env)
          fanoutPhase env snapshotUtxo ctx
        other ->
          throwIO $ ErrorCall ("Unexpected head status: " <> toString other)

  result <- try phases :: IO (Either SomeException ())

  roundEnd <- getCurrentTime
  modifyIORef' (ctxStatsRef ctx) $ \s -> s{roundEnd}

  case result of
    Right () -> pure ()
    Left ex -> handleRoundFailure env opts ctx roundNum ex

  readIORef (ctxStatsRef ctx)

handleRoundFailure :: FuzzyEnv -> FuzzyOptions -> RoundCtx -> Int -> SomeException -> IO ()
handleRoundFailure _env opts ctx roundNum ex = do
  let failType = classifyException ex
  putStrLn $ "Round " <> show roundNum <> " FAILED: " <> failureLabel failType
  modifyIORef' (ctxStatsRef ctx) $ \s -> s{roundFailure = Just failType}

  actionLog <- reverse <$> readIORef (ctxActionLogRef ctx)
  serverLog <- take 200 . reverse <$> readIORef (ctxServerLogRef ctx)

  let fr =
        FailureReport
          { frRound = roundNum
          , frFailureType = failType
          , frActionLog = actionLog
          , frServerOutputLog = serverLog
          , frPersistenceLogPaths =
              Map.fromList
                [ ("alice", "devnet/persistence/alice/")
                , ("bob", "devnet/persistence/bob/")
                , ("carol", "devnet/persistence/carol/")
                ]
          }
  reportPath <- writeFailureReport (fuzzyOutputDir opts) fr
  putStrLn $ "Failure report: " <> reportPath

classifyException :: SomeException -> FailureType
classifyException ex =
  let msg = show ex
   in if "UTxOMismatch" `isInfixOf` msg
        then UTxOMismatch
        else
          if "StuckSnapshot:" `isInfixOf` msg
            then parseStuck msg
            else
              if any (`isInfixOf` map toLower msg) ["connection", "websocket", "socket", "refused"]
                then NodeCrash msg
                else StuckSnapshot 0
 where
  parseStuck :: String -> FailureType
  parseStuck msg =
    let afterColon = drop 1 (dropWhile (/= ':') msg)
        secs = readMaybe (takeWhile (/= 's') afterColon) :: Maybe Int
     in StuckSnapshot (fromMaybe 0 secs)

-- * Logging helpers

logAction :: RoundCtx -> FuzzyParty -> ActionTag -> IO ()
logAction ctx party tag = do
  now <- getCurrentTime
  let entry = ActionEntry{aeTimestamp = now, aeActorId = partyId party, aeTag = tag}
  modifyIORef' (ctxActionLogRef ctx) (entry :)
  modifyIORef' (ctxStatsRef ctx) $ \s ->
    s{actionCounts = Map.insertWith (+) tag 1 (actionCounts s)}

logEvent :: RoundCtx -> Value -> IO ()
logEvent ctx v = do
  modifyIORef' (ctxServerLogRef ctx) (v :)
  case v ^? key "tag" . _String of
    Just "TxValid" ->
      modifyIORef' (ctxStatsRef ctx) $ \s -> s{txValidCount = txValidCount s + 1}
    Just "TxInvalid" -> do
      modifyIORef' (ctxStatsRef ctx) $ \s -> s{txInvalidCount = txInvalidCount s + 1}
      putStrLn $ "  TxInvalid: " <> show (v ^? key "validationError")
    Just "SnapshotConfirmed" -> do
      now <- getCurrentTime
      lastSnapshot <- readIORef (ctxLastSnapshotRef ctx)
      let latency = diffUTCTime now lastSnapshot
      writeIORef (ctxLastSnapshotRef ctx) now
      modifyIORef' (ctxStatsRef ctx) $ \s ->
        s
          { snapshotCount = snapshotCount s + 1
          , snapshotLatencies = latency : snapshotLatencies s
          }
      modifyIORef' (ctxPendingTxRef ctx) $ \n -> max 0 (n - 1)
    Just "CommitFinalized" ->
      modifyIORef' (ctxStatsRef ctx) $ \s -> s{depositsFinalized = depositsFinalized s + 1}
    Just "DepositExpired" ->
      modifyIORef' (ctxStatsRef ctx) $ \s -> s{depositsExpired = depositsExpired s + 1}
    Just "DecommitFinalized" ->
      modifyIORef' (ctxStatsRef ctx) $ \s -> s{decommitsFinalized = decommitsFinalized s + 1}
    _ -> pure ()

-- | Try to get one event from the WebSocket, returning Nothing on timeout/error.
tryGetEvent :: NominalDiffTime -> HydraClient -> IO (Maybe Value)
tryGetEvent t c =
  (try (waitMatch t c Just) :: IO (Either SomeException Value)) >>= \case
    Left _ -> pure Nothing
    Right v -> pure (Just v)

-- | Drain events from Alice's WebSocket for up to @totalSecs@ seconds.
drainEvents :: NominalDiffTime -> FuzzyEnv -> RoundCtx -> IO ()
drainEvents totalSecs env ctx = do
  deadline <- addUTCTime totalSecs <$> getCurrentTime
  go deadline
 where
  go deadline = do
    now <- getCurrentTime
    let remaining = diffUTCTime deadline now
    when (remaining > 0) $
      tryGetEvent (min remaining 1.0) (envAliceClient env) >>= \case
        Nothing -> pure ()
        Just v -> logEvent ctx v >> go deadline

-- * Stuck detection

checkStuck :: FuzzyOptions -> RoundCtx -> IO ()
checkStuck opts ctx = do
  pending <- readIORef (ctxPendingTxRef ctx)
  when (pending > 0) $ do
    lastSnapshot <- readIORef (ctxLastSnapshotRef ctx)
    now <- getCurrentTime
    let elapsed = diffUTCTime now lastSnapshot
    when (elapsed > fuzzyStuckTimeout opts) $ do
      let secs = floor elapsed :: Int
      throwIO $ ErrorCall ("StuckSnapshot:" <> show secs <> "s without SnapshotConfirmed")

-- * Init phase

initPhase :: FuzzyEnv -> RoundCtx -> IO ()
initPhase env ctx = do
  putStrLn "Init: sending Init..."
  send (envAliceClient env) (input "Init" [])
  waitForOpen (envAliceClient env) ctx

waitForOpen :: HydraClient -> RoundCtx -> IO ()
waitForOpen client ctx = do
  collectUntilOpen client ctx 0
  putStrLn "HeadIsOpen!"

commitParty :: FuzzyEnv -> FuzzyParty -> IO ()
commitParty env party = do
  let connectInfo = localNodeConnectInfo demoNetworkId (nodeSocket (envDirectOpts env))
  l1Utxo <- queryUTxOFor connectInfo QueryTip (fundsVkFor env party)
  if UTxO.null l1Utxo
    then putStrLn $ "  " <> show party <> ": no L1 UTxO, skipping commit"
    else do
      commitTx <-
        requestCommitTx (clientFor env party) l1Utxo
          <&> \tx -> withSecret (fundsSkFor env party) (`signTx` tx)
      runDirectBackend (envDirectOpts env) (submitTransaction commitTx)
      putStrLn $ "  " <> show party <> " commit submitted."

collectUntilOpen :: HydraClient -> RoundCtx -> Int -> IO ()
collectUntilOpen client ctx n = do
  v <- waitMatch 120 client $ \v ->
    case v ^? key "tag" . _String of
      Just "HeadIsOpen" -> Just v
      Just "CommitFinalized" -> Just v
      _ -> Nothing
  logEvent ctx v
  case v ^? key "tag" . _String of
    Just "HeadIsOpen" -> pure ()
    Just "CommitFinalized" -> do
      putStrLn $ "  CommitFinalized " <> show (n + 1)
      collectUntilOpen client ctx (n + 1)
    _ -> collectUntilOpen client ctx n

-- * Open phase

-- | Action weights: (action, weight)
actionWeights :: [(ActionTag, Int)]
actionWeights =
  [ (NewTxAction, 65)
  , (DepositAction, 15)
  , (DecommitAction, 10)
  , (WaitExpireAction, 5)
  , (CloseAction, 5)
  ]

totalWeight :: Int
totalWeight = sum (map snd actionWeights)

pickWeightedAction :: IORef StdGen -> IO ActionTag
pickWeightedAction rngRef = do
  rng <- readIORef rngRef
  let (n, rng') = randomR (0, totalWeight - 1) rng
  writeIORef rngRef rng'
  pure $ go n actionWeights
 where
  go :: Int -> [(ActionTag, Int)] -> ActionTag
  go _ [] = CloseAction
  go n ((tag, w) : rest)
    | n < w = tag
    | otherwise = go (n - w) rest

pickRandom :: IORef StdGen -> [a] -> IO a
pickRandom rngRef xs = do
  rng <- readIORef rngRef
  let (n, rng') = randomR (0, length xs - 1) rng
  writeIORef rngRef rng'
  pure (xs !! n)

openPhase :: FuzzyEnv -> FuzzyOptions -> RoundCtx -> IO UTxO
openPhase env opts ctx = do
  putStrLn "Open phase: seeding L2 with initial deposit..."
  doDeposit env ctx
  if fuzzyStress opts
    then do
      putStrLn $ "Stress mode: " <> show (fuzzyParallelism opts) <> " parallel senders, " <> show (fuzzyTxsPerRound opts) <> " txs each..."
      forConcurrently_ [1 .. fuzzyParallelism opts] $ \_ ->
        replicateM_ (fuzzyTxsPerRound opts) (doNewTx env ctx opts)
      drainEvents 10 env ctx
    else go 0 (fuzzyTxsPerRound opts)
  getSnapshotUTxO (envAliceClient env)
 where
  -- Don't allow Close until at least this many actions have run
  minBeforeClose = max 5 (fuzzyTxsPerRound opts `div` 5)

  go :: Int -> Int -> IO ()
  go _ 0 = putStrLn "Reached txs-per-round limit."
  go done remaining = do
    checkStuck opts ctx
    action <- pickWeightedAction (ctxRngRef ctx)
    case action of
      CloseAction
        | done < minBeforeClose ->
            -- Too early for Close; skip this pick and try again
            go done remaining
      CloseAction -> putStrLn "Picked Close, ending open phase."
      _ -> do
        executeAction env ctx opts action
        drainEvents 2 env ctx
        go (done + 1) (remaining - 1)

executeAction :: FuzzyEnv -> RoundCtx -> FuzzyOptions -> ActionTag -> IO ()
executeAction env ctx opts = \case
  NewTxAction -> doNewTx env ctx opts
  DepositAction -> doDeposit env ctx
  DecommitAction -> doDecommit env ctx opts
  WaitExpireAction -> doWaitExpire env ctx opts
  _ -> pure ()

doNewTx :: FuzzyEnv -> RoundCtx -> FuzzyOptions -> IO ()
doNewTx env ctx opts = do
  l2Utxo <- getSnapshotUTxO (envAliceClient env)
  case findSpendableUtxo env l2Utxo of
    Nothing -> do
      putStrLn "  NewTx: no spendable L2 UTxO, seeding via deposit instead..."
      doDeposit env ctx
    Just (party, txIn, txOut) -> do
      let recipient = nextParty party
      let recipientAddr = mkVkAddress demoNetworkId (fundsVkFor env recipient)
      -- Split: send half to recipient, keep half as change (enables parallel spending)
      let totalLovelace = selectLovelace (txOutValue txOut)
          minUTxO = 2_000_000
          sendValue =
            if totalLovelace > 2 * minUTxO
              then lovelaceToValue (totalLovelace `div` 2)
              else txOutValue txOut
      case mkSimpleTx (txIn, txOut) (recipientAddr, sendValue) (fundsSkFor env party) of
        Left err -> putStrLn $ "  NewTx: build failed: " <> show err
        Right tx -> do
          send (clientFor env party) (input "NewTx" ["transaction" .= Aeson.toJSON tx])
          logAction ctx party NewTxAction
          modifyIORef' (ctxPendingTxRef ctx) (+ 1)
          putStrLn $ "  NewTx sent (" <> show party <> " → " <> show recipient <> " " <> show (selectLovelace sendValue) <> " lovelace), awaiting TxValid..."
          outcome <- waitMatch 30 (envAliceClient env) $ \v ->
            v
              ^? key "tag" . _String >>= \case
                "TxValid" -> Just (Right v)
                "TxInvalid" -> Just (Left v)
                _ -> Nothing
          case outcome of
            Left v -> do
              logEvent ctx v
              modifyIORef' (ctxPendingTxRef ctx) (\n -> max 0 (n - 1))
              putStrLn "  NewTx: rejected (TxInvalid)"
            Right v -> do
              logEvent ctx v
              putStrLn "  NewTx: accepted — awaiting SnapshotConfirmed..."
              sv <- waitMatch (fuzzyStuckTimeout opts) (envAliceClient env) $ \msg ->
                msg
                  ^? key "tag" . _String >>= \case
                    "SnapshotConfirmed" -> Just msg
                    _ -> Nothing
              logEvent ctx sv
              putStrLn "  NewTx: confirmed in snapshot"

doDeposit :: FuzzyEnv -> RoundCtx -> IO ()
doDeposit env ctx = do
  party <- pickRandom (ctxRngRef ctx) [FuzzyAlice, FuzzyBob, FuzzyCarol]
  let connectInfo = localNodeConnectInfo demoNetworkId (nodeSocket (envDirectOpts env))
  l1Utxo <- queryUTxOFor connectInfo QueryTip (fundsVkFor env party)
  if UTxO.null l1Utxo
    then putStrLn $ "  Deposit: " <> show party <> " has no L1 funds, skipping"
    else do
      commitTx <-
        requestCommitTx (clientFor env party) l1Utxo
          <&> \tx -> withSecret (fundsSkFor env party) (`signTx` tx)
      runDirectBackend (envDirectOpts env) (submitTransaction commitTx)
      logAction ctx party DepositAction
      putStrLn $ "  Deposit submitted (" <> show party <> "), awaiting CommitFinalized..."
      v <- waitMatch 120 (envAliceClient env) $ \msg ->
        msg
          ^? key "tag" . _String >>= \case
            "CommitFinalized" -> Just msg
            _ -> Nothing
      logEvent ctx v
      putStrLn $ "  Deposit confirmed (" <> show party <> ") — funds now on L2"

doWaitExpire :: FuzzyEnv -> RoundCtx -> FuzzyOptions -> IO ()
doWaitExpire env ctx opts = do
  party <- pickRandom (ctxRngRef ctx) [FuzzyAlice, FuzzyBob, FuzzyCarol]
  let connectInfo = localNodeConnectInfo demoNetworkId (nodeSocket (envDirectOpts env))
  l1Utxo <- queryUTxOFor connectInfo QueryTip (fundsVkFor env party)
  if UTxO.null l1Utxo
    then putStrLn $ "  WaitExpire: " <> show party <> " has no L1 funds, skipping"
    else do
      commitTx <-
        requestCommitTx (clientFor env party) l1Utxo
          <&> \tx -> withSecret (fundsSkFor env party) (`signTx` tx)
      let depositId = getTxId (getTxBody commitTx)
      runDirectBackend (envDirectOpts env) (submitTransaction commitTx)
      logAction ctx party WaitExpireAction
      putStrLn $ "  WaitExpire: deposit submitted, waiting ~" <> show (floor (fuzzyDepositExpireWait opts) :: Int) <> "s for expiry..."
      outcome <- waitMatch (fuzzyDepositExpireWait opts + 30) (envAliceClient env) $ \msg ->
        msg
          ^? key "tag" . _String >>= \case
            "DepositExpired" -> Just (Left msg)
            "CommitFinalized" -> Just (Right msg)
            _ -> Nothing
      case outcome of
        Right v -> do
          logEvent ctx v
          putStrLn "  WaitExpire: deposit was activated before expiry (head was fast)"
        Left v -> do
          logEvent ctx v
          putStrLn "  WaitExpire: expired, recovering..."
          let Host{hostname, port} = fuzzyAliceHost opts
          void $
            parseUrlThrow ("DELETE http://" <> toString hostname <> ":" <> show port <> "/commits/" <> show depositId)
              >>= httpLbs
          putStrLn "  WaitExpire: recover submitted"

doDecommit :: FuzzyEnv -> RoundCtx -> FuzzyOptions -> IO ()
doDecommit env ctx opts = do
  l2Utxo <- getSnapshotUTxO (envAliceClient env)
  case findSpendableUtxo env l2Utxo of
    Nothing -> putStrLn "  Decommit: no spendable L2 UTxO, skipping"
    Just (party, txIn, txOut) -> do
      let selfAddr = mkVkAddress demoNetworkId (fundsVkFor env party)
      case mkSimpleTx (txIn, txOut) (selfAddr, txOutValue txOut) (fundsSkFor env party) of
        Left err -> putStrLn $ "  Decommit: build failed: " <> show err
        Right tx -> do
          postDecommit (clientFor env party) tx
          logAction ctx party DecommitAction
          putStrLn $ "  Decommit submitted (" <> show party <> "), awaiting DecommitFinalized..."
          v <- waitMatch (fuzzyStuckTimeout opts + 60) (envAliceClient env) $ \msg ->
            msg
              ^? key "tag" . _String >>= \case
                "DecommitFinalized" -> Just msg
                _ -> Nothing
          logEvent ctx v
          putStrLn "  Decommit finalized"

-- | Find a UTxO in the snapshot owned by one of the parties with enough lovelace.
findSpendableUtxo ::
  FuzzyEnv ->
  UTxO ->
  Maybe (FuzzyParty, TxIn, TxOut CAPI.CtxUTxO)
findSpendableUtxo env utxo =
  listToMaybe
    [ (party, txIn, txOut)
    | party <- [FuzzyAlice, FuzzyBob, FuzzyCarol]
    , let addr = mkVkAddress demoNetworkId (fundsVkFor env party)
    , (txIn, txOut) <- UTxO.toList utxo
    , txOutAddress txOut == addr
    , selectLovelace (txOutValue txOut) > 2_000_000
    ]

nextParty :: FuzzyParty -> FuzzyParty
nextParty FuzzyAlice = FuzzyBob
nextParty FuzzyBob = FuzzyCarol
nextParty FuzzyCarol = FuzzyAlice

-- * Close phase

closePhase :: FuzzyEnv -> FuzzyOptions -> RoundCtx -> IO ()
closePhase env opts ctx = do
  putStrLn "Close phase: sending Close..."
  sendCloseAndWait env ctx 5

  -- 30% chance to Contest from Bob or Carol
  rng <- readIORef (ctxRngRef ctx)
  let (n, rng') = randomR (0 :: Int, 9) rng
  writeIORef (ctxRngRef ctx) rng'
  when (n < 3) $ do
    contestant <- pickRandom (ctxRngRef ctx) [FuzzyBob, FuzzyCarol]
    putStrLn $ "Contesting from " <> show contestant <> "..."
    send (clientFor env contestant) (input "Contest" [])
    logAction ctx contestant ContestAction
    drainEvents 5 env ctx

  -- Wait for ReadyToFanout (contestation period + buffer)
  let waitBudget = 120 + floor (fuzzyStuckTimeout opts) :: Int
  putStrLn $ "Waiting for ReadyToFanout (up to " <> show waitBudget <> "s)..."
  _ <- waitMatch (fromIntegral waitBudget) (envAliceClient env) $ \v ->
    guard (v ^? key "tag" . _String == Just "ReadyToFanout") $> v
  putStrLn "ReadyToFanout."

sendCloseAndWait :: FuzzyEnv -> RoundCtx -> Int -> IO ()
sendCloseAndWait env ctx retriesLeft = do
  send (envAliceClient env) (input "Close" [])
  logAction ctx FuzzyAlice CloseAction
  outcome <-
    waitMatch 120 (envAliceClient env) $ \v ->
      case v ^? key "tag" . _String of
        Just "HeadIsClosed" -> Just (Right ())
        Just "PostTxOnChainFailed" -> Just (Left (v ^? key "postTxError" . _String))
        _ -> Nothing
  case outcome of
    Right () -> putStrLn "HeadIsClosed."
    Left mbErr
      | retriesLeft > 0 -> do
          putStrLn $
            "Close tx failed ("
              <> maybe "unknown error" toString mbErr
              <> "), retrying in 5s ("
              <> show retriesLeft
              <> " retries left)..."
          threadDelay 5_000_000
          sendCloseAndWait env ctx (retriesLeft - 1)
      | otherwise ->
          throwIO $
            ErrorCall $
              "Close tx failed after all retries: "
                <> maybe "unknown error" toString mbErr

-- * Fanout phase

fanoutPhase :: FuzzyEnv -> UTxO -> RoundCtx -> IO ()
fanoutPhase env snapshotUtxo ctx = do
  let snapshotLovelace = UTxO.totalLovelace snapshotUtxo

  putStrLn "Fanout phase: sending Fanout..."
  send (envAliceClient env) (input "Fanout" [])
  logAction ctx FuzzyAlice FanoutAction

  finalOutputs <-
    waitMatch 120 (envAliceClient env) $ \v -> do
      guard $ v ^? key "tag" . _String == Just "HeadIsFinalized"
      v ^? key "finalizedUTxO" . _JSON :: Maybe [CAPI.TxOut CAPI.CtxUTxO]

  let finalLovelace = foldMap (selectLovelace . txOutValue) finalOutputs
  -- finalized >= snapshot is normal: pending deposits add lovelace
  -- finalized < snapshot would mean value was destroyed (a real bug)
  let utxoOk = finalLovelace >= snapshotLovelace

  modifyIORef' (ctxStatsRef ctx) $ \s -> s{utxoCheckPassed = utxoOk}

  if utxoOk
    then
      putStrLn $
        "UTxO check PASSED (snapshot="
          <> show snapshotLovelace
          <> " finalized="
          <> show finalLovelace
          <> ")"
    else do
      putStrLn $
        "UTxO check FAILED: value destroyed! snapshot="
          <> show snapshotLovelace
          <> " finalized="
          <> show finalLovelace
      throwIO $ ErrorCall "UTxOMismatch"
