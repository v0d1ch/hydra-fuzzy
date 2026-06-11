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
import Data.Aeson.Lens (key, _String)
import Data.Char (toLower)
import Data.List (isInfixOf, isPrefixOf, maximumBy, minimumBy, minimum, sortBy, (!!))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
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
  serialiseToRawBytesHexText,
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
  , fuzzyFloodDuration :: NominalDiffTime
  , fuzzyExpandUtxo :: Int
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
    , fuzzyTxsPerRound = 200
    , fuzzyStress = False
    , fuzzyParallelism = 10
    , fuzzyFloodDuration = 60
    , fuzzyExpandUtxo = 6
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
  -- The Hydra node uses each party's cardano signing key (not funds key) for
  -- fee payment when building commit txs. We need to check that wallet too.
  , envAliceCardanoVk :: VerificationKey PaymentKey
  , envBobCardanoVk :: VerificationKey PaymentKey
  , envCarolCardanoVk :: VerificationKey PaymentKey
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

cardanoVkFor :: FuzzyEnv -> FuzzyParty -> VerificationKey PaymentKey
cardanoVkFor env = \case
  FuzzyAlice -> envAliceCardanoVk env
  FuzzyBob -> envBobCardanoVk env
  FuzzyCarol -> envCarolCardanoVk env

-- * Entry point

runFuzzy :: FuzzyOptions -> IO ()
runFuzzy opts = do
  (aliceFundsVk, aliceFundsSk) <- keysFor AliceFunds
  (bobFundsVk, bobFundsSk) <- keysFor BobFunds
  (carolFundsVk, carolFundsSk) <- keysFor CarolFunds
  -- The Hydra node uses each party's cardano signing key for fee payment when
  -- building commit txs. Load their VKs so we can guard against too-small fees.
  (aliceCardanoVk, _) <- keysFor Alice
  (bobCardanoVk, _) <- keysFor Bob
  (carolCardanoVk, _) <- keysFor Carol

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
          , envAliceCardanoVk = aliceCardanoVk
          , envBobCardanoVk = bobCardanoVk
          , envCarolCardanoVk = carolCardanoVk
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
  , ctxPendingDepositsRef :: IORef Int
  , ctxRngRef :: IORef StdGen
  , ctxInFlightRef :: IORef (Set TxIn)
  }

newRoundCtx :: Int -> UTCTime -> IORef StdGen -> IO RoundCtx
newRoundCtx n now rngRef = do
  ctxStatsRef <- newIORef (emptyRoundStats n now)
  ctxActionLogRef <- newIORef []
  ctxServerLogRef <- newIORef []
  ctxLastSnapshotRef <- newIORef now
  ctxPendingTxRef <- newIORef 0
  ctxPendingDepositsRef <- newIORef 0
  ctxInFlightRef <- newIORef mempty
  pure
    RoundCtx
      { ctxStatsRef
      , ctxActionLogRef
      , ctxServerLogRef
      , ctxLastSnapshotRef
      , ctxPendingTxRef
      , ctxPendingDepositsRef
      , ctxRngRef = rngRef
      , ctxInFlightRef
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

  l1Before <- totalL1Lovelace env
  modifyIORef' (ctxStatsRef ctx) $ \s -> s{l1LovelaceBefore = l1Before}
  putStrLn $ "L1 balance snapshot: " <> show l1Before <> " lovelace"

  headStatus <- getHeadStatus (envAliceClient env)
  putStrLn $ "Head status: " <> toString headStatus

  let phases = case headStatus of
        "Idle" ->
          initPhase env ctx
            >> openPhase env opts ctx
            >>= \snapshotUtxo -> closePhase env opts ctx >> fanoutPhase env snapshotUtxo ctx
        "Open" -> do
          -- If L2 already has many UTxOs, skip openPhase to avoid draining them.
          -- We want to close with a large UTxO set to stress the partial fanout path.
          snapshotUtxo0 <- getSnapshotUTxO (envAliceClient env)
          let l2Count = Map.size (UTxO.toMap snapshotUtxo0)
          if l2Count > 50
            then do
              putStrLn $ "Open phase: skipping (L2 already has " <> show l2Count <> " UTxOs, going straight to close)"
              closePhase env opts ctx
              fanoutPhase env snapshotUtxo0 ctx
            else
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

  -- Compute TPS: valid txs / round duration (roundEnd is the local binding above)
  stats0 <- readIORef (ctxStatsRef ctx)
  let dur = realToFrac (diffUTCTime roundEnd (roundStart stats0)) :: Double
      tps = if dur > 0 then fromIntegral (txValidCount stats0) / dur else 0.0
  modifyIORef' (ctxStatsRef ctx) $ \s -> s{roundThroughputTps = tps}

  readIORef (ctxStatsRef ctx)

handleRoundFailure :: FuzzyEnv -> FuzzyOptions -> RoundCtx -> Int -> SomeException -> IO ()
handleRoundFailure _env opts ctx roundNum ex = do
  let rawMsg = show ex
      failType = classifyException rawMsg
  putStrLn $ "Round " <> show roundNum <> " FAILED: " <> failureLabel failType
  modifyIORef' (ctxStatsRef ctx) $ \s -> s{roundFailure = Just failType}

  actionLog <- reverse <$> readIORef (ctxActionLogRef ctx)
  serverLog <- take 200 . reverse <$> readIORef (ctxServerLogRef ctx)

  let fr =
        FailureReport
          { frRound = roundNum
          , frFailureType = failType
          , frRawException = rawMsg
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

classifyException :: String -> FailureType
classifyException msg =
  let msgLower = map toLower msg
   in if "UTxOMismatch" `isInfixOf` msg
        then UTxOMismatch
        else
          if "FanoutFailed:" `isInfixOf` msg
            then FanoutFailed (drop 1 (dropWhile (/= ':') msg))
            else
              if "L1ValueLoss:" `isInfixOf` msg
                then parseL1Loss msg
                else
                  if "StuckSnapshot:" `isInfixOf` msg
                    then parseStuck msg
                    else
                      if "waitMatch did not match" `isInfixOf` msg
                        then parseWaitMatchTimeout msg
                        else
                          if any (`isInfixOf` msgLower) ["waitnext:", "connectionclosed", "connectionabruptly", "handshakeexception"]
                            then NodeCrash msg
                            else
                              if any (`isInfixOf` msgLower) ["connection refused", "websocket", "socket does not exist"]
                                then NodeCrash msg
                                else -- Unknown exception: preserve message for debugging
                                  TransientError msg
 where
  parseStuck :: String -> FailureType
  parseStuck m =
    let afterColon = drop 1 (dropWhile (/= ':') m)
        secs = readMaybe (takeWhile (/= 's') afterColon) :: Maybe Int
     in StuckSnapshot (fromMaybe 0 secs)

  parseL1Loss :: String -> FailureType
  parseL1Loss m =
    let afterColon = drop 1 (dropWhile (/= ':') m)
        n = readMaybe (takeWhile (/= ' ') afterColon) :: Maybe Integer
     in L1ValueLoss (fromMaybe 0 n)

  parseWaitMatchTimeout :: String -> FailureType
  parseWaitMatchTimeout m =
    let findAfter [] = []
        findAfter s@(_ : rest)
          | "within " `isPrefixOf` s = drop 7 s
          | otherwise = findAfter rest
        timeStr = findAfter m
        secs = readMaybe (takeWhile (\c -> c == '.' || (c >= '0' && c <= '9')) timeStr) :: Maybe Double
     in StuckSnapshot (maybe 0 round secs)

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
      writeIORef (ctxInFlightRef ctx) mempty
    Just "CommitFinalized" -> do
      modifyIORef' (ctxStatsRef ctx) $ \s -> s{depositsFinalized = depositsFinalized s + 1}
      modifyIORef' (ctxPendingDepositsRef ctx) $ \n -> max 0 (n - 1)
    Just "DepositExpired" -> do
      modifyIORef' (ctxStatsRef ctx) $ \s -> s{depositsExpired = depositsExpired s + 1}
      modifyIORef' (ctxPendingDepositsRef ctx) $ \n -> max 0 (n - 1)
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

-- | Action weights for sequential (non-stress) mode.
actionWeights :: [(ActionTag, Int)]
actionWeights =
  [ (NewTxAction, 42)
  , (MultiSendAction, 18)
  , (DepositAction, 12)
  , (DecommitAction, 10)
  , (DrainAllAction, 0)
  , (WaitExpireAction, 5)
  , (CloseAction, 1)
  , (ForceCloseAction, 80)
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
  seedL2 env ctx
  if fuzzyStress opts
    then do
      -- Phase 1: fan out UTxOs so parallel senders have many inputs
      putStrLn $ "Stress mode: expanding UTxO set (target " <> show (fuzzyExpandUtxo opts) <> " per party)..."
      expandUtxo env opts ctx
      -- Phase 2: flood for configured duration
      let secs = floor (fuzzyFloodDuration opts) :: Int
      putStrLn $ "Stress mode: flooding " <> show secs <> "s \xd7 " <> show (fuzzyParallelism opts) <> " senders..."
      floodForDuration opts env ctx
      drainEvents 60 env ctx
      -- Phase 3: exercise decommits
      putStrLn "Stress mode: exercising decommits..."
      replicateM_ (fuzzyParallelism opts `div` 2 + 1) (doDecommit env ctx opts)
      drainEvents 20 env ctx
    else go 0 (fuzzyTxsPerRound opts)
  getSnapshotUTxO (envAliceClient env)
 where
  minBeforeClose = max 20 (fuzzyTxsPerRound opts * 2 `div` 3)

  go :: Int -> Int -> IO ()
  go _ 0 = putStrLn "Reached txs-per-round limit."
  go done remaining = do
    checkStuck opts ctx
    action <- pickWeightedAction (ctxRngRef ctx)
    case action of
      ForceCloseAction
        | done < minBeforeClose -> go done remaining
      ForceCloseAction -> do
        -- Pump UTxOs to ~60 (20 per party) so partial fanout always runs across
        -- multiple batches, exercising the accumulator-commitment code path.
        putStrLn "ForceClose: expanding UTxO set for partial fanout stress..."
        expandUtxo env opts{fuzzyExpandUtxo = 20} ctx
        -- Submit deposits from every party so all three are pending at close time.
        putStrLn "ForceClose: submitting deposits from all parties..."
        mapM_ (\p -> doDepositFor p env ctx) [FuzzyAlice, FuzzyBob, FuzzyCarol]
        -- Also fire an async decommit — decommits modify the snapshot UTxO set,
        -- making accumulator mismatches on partial fanout more likely to surface.
        doDecommitAsync env ctx
        pendingDeps' <- readIORef (ctxPendingDepositsRef ctx)
        pendingTxs <- readIORef (ctxPendingTxRef ctx)
        putStrLn $
          "ForceClose: closing now ("
            <> show pendingDeps'
            <> " deposit(s), "
            <> show pendingTxs
            <> " tx(s) still pending)"
        logAction ctx FuzzyAlice ForceCloseAction
      CloseAction
        | done < minBeforeClose -> go done remaining
      CloseAction -> putStrLn "Picked Close, ending open phase."
      _ -> do
        executeAction env ctx opts action
        drainEvents 2 env ctx
        go (done + 1) (remaining - 1)

executeAction :: FuzzyEnv -> RoundCtx -> FuzzyOptions -> ActionTag -> IO ()
executeAction env ctx opts = \case
  NewTxAction -> doNewTx env ctx opts
  MultiSendAction -> doMultiSend env ctx opts
  DepositAction -> doDeposit env ctx
  DecommitAction -> doDecommit env ctx opts
  DrainAllAction -> doDrainAll env ctx opts
  WaitExpireAction -> doWaitExpire env ctx opts
  _ -> pure ()  -- CloseAction / ForceCloseAction handled in openPhase

-- * UTxO expansion (stress mode pre-flood)

-- | Fan out UTxOs so every party reaches the target count.
-- Picks the richest party as sender and the poorest as recipient to balance
-- the UTxO distribution evenly. Waits for SnapshotConfirmed each iteration.
expandUtxo :: FuzzyEnv -> FuzzyOptions -> RoundCtx -> IO ()
expandUtxo env opts ctx = go (0 :: Int)
 where
  target = fuzzyExpandUtxo opts
  maxIter = target * 9

  parties :: [FuzzyParty]
  parties = [FuzzyAlice, FuzzyBob, FuzzyCarol]

  utxoCountFor :: UTxO -> FuzzyParty -> Int
  utxoCountFor utxo party =
    length
      [ ()
      | (_, txOut) <- UTxO.toList utxo
      , txOutAddress txOut == mkVkAddress demoNetworkId (fundsVkFor env party)
      ]

  go n = do
    l2Utxo <- getSnapshotUTxO (envAliceClient env)
    let withCounts = [(utxoCountFor l2Utxo p, p) | p <- parties]
        counts = map fst withCounts
        minC = minimum counts
    if minC >= target || n >= maxIter
      then
        putStrLn $
          "  Expand done: "
            <> intercalate "/" (map show counts)
            <> " UTxOs (Alice/Bob/Carol)"
      else do
        -- Send from richest → poorest to spread UTxOs evenly
        let richest = snd $ maximumBy (\(a, _) (b, _) -> compare a b) withCounts
            poorest = snd $ minimumBy (\(a, _) (b, _) -> compare a b) withCounts
            -- If they're the same party, fall back to nextParty
            recipient = if richest == poorest then nextParty richest else poorest
        -- Find a splittable UTxO owned by the richest party
        let richAddr = mkVkAddress demoNetworkId (fundsVkFor env richest)
            splittable =
              [ (txIn, txOut)
              | (txIn, txOut) <- UTxO.toList l2Utxo
              , txOutAddress txOut == richAddr
              , selectLovelace (txOutValue txOut) > 4_000_000
              ]
        case splittable of
          [] -> do
            -- Richest has nothing big enough; try anyone via claimUtxo
            claimUtxo env ctx l2Utxo >>= \case
              Nothing -> putStrLn "  Expand: nothing claimable, stopping"
              Just (party, txIn, txOut) -> do
                atomicModifyIORef' (ctxInFlightRef ctx) (\s -> (Set.delete txIn s, ()))
                go (n + 1)
          (txIn, txOut) : _ -> do
            mClaimed <- atomicModifyIORef' (ctxInFlightRef ctx) $ \inFlight ->
              if txIn `Set.member` inFlight
                then (inFlight, Nothing)
                else (Set.insert txIn inFlight, Just (txIn, txOut))
            case mClaimed of
              Nothing -> go (n + 1)
              Just (claimedIn, claimedOut) -> do
                let totalLovelace = selectLovelace (txOutValue claimedOut)
                    recipientAddr = mkVkAddress demoNetworkId (fundsVkFor env recipient)
                    sendVal = lovelaceToValue (totalLovelace `div` 2)
                case mkSimpleTx (claimedIn, claimedOut) (recipientAddr, sendVal) (fundsSkFor env richest) of
                  Left err -> do
                    putStrLn $ "  Expand: build failed: " <> show err
                    atomicModifyIORef' (ctxInFlightRef ctx) (\s -> (Set.delete claimedIn s, ()))
                    go (n + 1)
                  Right tx -> do
                    send (clientFor env richest) (input "NewTx" ["transaction" .= Aeson.toJSON tx])
                    logAction ctx richest NewTxAction
                    modifyIORef' (ctxPendingTxRef ctx) (+ 1)
                    sv <- waitMatch (fuzzyStuckTimeout opts) (envAliceClient env) $ \msg ->
                      msg ^? key "tag" . _String >>= \case
                        "SnapshotConfirmed" -> Just msg
                        _ -> Nothing
                    logEvent ctx sv
                    go (n + 1)

-- | Time-bounded tx flood: N threads, each sending as fast as possible.
-- Records flood-window peak TPS into ctxStatsRef.
floodForDuration :: FuzzyOptions -> FuzzyEnv -> RoundCtx -> IO ()
floodForDuration opts env ctx = do
  floodStart <- getCurrentTime
  txBefore <- txValidCount <$> readIORef (ctxStatsRef ctx)
  forConcurrently_ [1 .. fuzzyParallelism opts] $ \_ ->
    let loop = do
          now <- getCurrentTime
          when (diffUTCTime now floodStart < fuzzyFloodDuration opts) $
            sendNewTx env ctx >> loop
     in loop
  floodEnd <- getCurrentTime
  txAfter <- txValidCount <$> readIORef (ctxStatsRef ctx)
  let floodDur = realToFrac (diffUTCTime floodEnd floodStart) :: Double
      peakTps = if floodDur > 0 then fromIntegral (txAfter - txBefore) / floodDur else 0.0
  modifyIORef' (ctxStatsRef ctx) $ \s -> s{floodPeakTps = peakTps}

-- | Fire-and-forget variant: claim a UTxO, send it, increment pending counter.
-- Concurrent threads use ctxInFlightRef to avoid double-spending.
sendNewTx :: FuzzyEnv -> RoundCtx -> IO ()
sendNewTx env ctx = do
  l2Utxo <- getSnapshotUTxO (envAliceClient env)
  let candidates =
        [ (party, txIn, txOut)
        | party <- [FuzzyAlice, FuzzyBob, FuzzyCarol]
        , let addr = mkVkAddress demoNetworkId (fundsVkFor env party)
        , (txIn, txOut) <- UTxO.toList l2Utxo
        , txOutAddress txOut == addr
        , selectLovelace (txOutValue txOut) >= 2_000_000
        ]
  mClaimed <- atomicModifyIORef' (ctxInFlightRef ctx) $ \inFlight ->
    case filter (\(_, txIn, _) -> txIn `Set.notMember` inFlight) candidates of
      [] -> (inFlight, Nothing)
      (party, txIn, txOut) : _ -> (Set.insert txIn inFlight, Just (party, txIn, txOut))
  case mClaimed of
    Nothing -> pure ()
    Just (party, txIn, txOut) -> do
      let recipient = nextParty party
          recipientAddr = mkVkAddress demoNetworkId (fundsVkFor env recipient)
          totalLovelace = selectLovelace (txOutValue txOut)
          minUTxO = 2_000_000
      -- Randomise the send fraction (30%–70%) so UTxO amounts vary across rounds
      fraction <- atomicModifyIORef' (ctxRngRef ctx) $ \rng ->
        let (pct, rng') = randomR (30 :: Int, 70) rng in (rng', pct)
      let sendValue =
            if totalLovelace > 2 * minUTxO
              then
                let proposed = max minUTxO (totalLovelace * fromIntegral fraction `div` 100)
                    change = totalLovelace - proposed
                 in if change >= minUTxO
                      then lovelaceToValue proposed
                      else txOutValue txOut
              else txOutValue txOut
      case mkSimpleTx (txIn, txOut) (recipientAddr, sendValue) (fundsSkFor env party) of
        Left _ -> atomicModifyIORef' (ctxInFlightRef ctx) (\s -> (Set.delete txIn s, ()))
        Right tx -> do
          send (clientFor env party) (input "NewTx" ["transaction" .= Aeson.toJSON tx])
          logAction ctx party NewTxAction
          modifyIORef' (ctxPendingTxRef ctx) (+ 1)

-- | Sequential NewTx with explicit SnapshotConfirmed wait (used in sequential mode).
doNewTx :: FuzzyEnv -> RoundCtx -> FuzzyOptions -> IO ()
doNewTx env ctx opts = do
  l2Utxo <- getSnapshotUTxO (envAliceClient env)
  claimUtxo env ctx l2Utxo >>= \case
    Nothing -> do
      putStrLn "  NewTx: no spendable L2 UTxO, seeding via deposit instead..."
      doDeposit env ctx
    Just (party, txIn, txOut) -> do
      let recipient = nextParty party
          recipientAddr = mkVkAddress demoNetworkId (fundsVkFor env recipient)
          totalLovelace = selectLovelace (txOutValue txOut)
          minUTxO = 2_000_000
      fraction <- atomicModifyIORef' (ctxRngRef ctx) $ \rng ->
        let (pct, rng') = randomR (30 :: Int, 70) rng in (rng', pct)
      let sendValue =
            if totalLovelace > 2 * minUTxO
              then
                let proposed = max minUTxO (totalLovelace * fromIntegral fraction `div` 100)
                    change = totalLovelace - proposed
                 in if change >= minUTxO
                      then lovelaceToValue proposed
                      else txOutValue txOut
              else txOutValue txOut
      case mkSimpleTx (txIn, txOut) (recipientAddr, sendValue) (fundsSkFor env party) of
        Left err -> putStrLn $ "  NewTx: build failed: " <> show err
        Right tx -> do
          send (clientFor env party) (input "NewTx" ["transaction" .= Aeson.toJSON tx])
          logAction ctx party NewTxAction
          modifyIORef' (ctxPendingTxRef ctx) (+ 1)
          putStrLn $ "  NewTx sent (" <> show party <> " \x2192 " <> show recipient <> " " <> show (selectLovelace sendValue) <> " lovelace), awaiting TxValid..."
          outcome <- waitMatch (fuzzyStuckTimeout opts) (envAliceClient env) $ \v ->
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
              putStrLn "  NewTx: accepted \x2014 awaiting SnapshotConfirmed..."
              sv <- waitMatch (fuzzyStuckTimeout opts) (envAliceClient env) $ \msg ->
                msg
                  ^? key "tag" . _String >>= \case
                    "SnapshotConfirmed" -> Just msg
                    _ -> Nothing
              logEvent ctx sv
              putStrLn "  NewTx: confirmed in snapshot"

-- | Burst: fire N txs rapid-fire (fire-and-forget each), then let event drain collect them.
doMultiSend :: FuzzyEnv -> RoundCtx -> FuzzyOptions -> IO ()
doMultiSend env ctx opts = do
  let burstSize = 5 :: Int
  putStrLn $ "  MultiSend: firing " <> show burstSize <> " txs rapid-fire..."
  logAction ctx FuzzyAlice MultiSendAction
  replicateM_ burstSize (sendNewTx env ctx)
  -- Collect confirmations immediately so pending count doesn't inflate
  drainEvents (min 20 (fuzzyStuckTimeout opts)) env ctx
  pending <- readIORef (ctxPendingTxRef ctx)
  putStrLn $ "  MultiSend: done (" <> show pending <> " still pending)"

-- | Retry an IO action on HTTP/connection exceptions with fixed backoff.
retryOnHttpError :: Int -> IO a -> IO a
retryOnHttpError 0 act = act
retryOnHttpError n act =
  act `catch` \(ex :: SomeException) ->
    if isHttpErr (show ex)
      then threadDelay 500_000 >> retryOnHttpError (n - 1) act
      else throwIO ex
 where
  isHttpErr msg =
    any
      (`isInfixOf` map toLower msg)
      ["httpexception", "connection refused", "vanillahttpexception", "network.socket", "responsetimeout", "connectiontimeout"]

-- | Seed L2 with deposits from all parties that have L1 funds.
-- Returns as soon as the first CommitFinalized arrives.
seedL2 :: FuzzyEnv -> RoundCtx -> IO ()
seedL2 env ctx = do
  l2Utxo <- getSnapshotUTxO (envAliceClient env)
  case findSpendableUtxo env l2Utxo of
    Just _ -> putStrLn "  Seed: L2 already has spendable UTxO, skipping deposits"
    Nothing -> do
      let connectInfo = localNodeConnectInfo demoNetworkId (nodeSocket (envDirectOpts env))
      partyFunds <- forM [FuzzyAlice, FuzzyBob, FuzzyCarol] $ \party -> do
        l1Utxo <- queryUTxOFor connectInfo QueryTip (fundsVkFor env party)
        pure (party, l1Utxo)
      -- Find the party with the LARGEST eligible UTxO to maximise expand potential.
      -- Try Alice first; fall back to whoever has the most ADA. The concurrent-commit
      -- race only applies during Init (not after the head is Open), so any party is safe.
      let allEligible =
            [ (party, txIn, txOut)
            | (party, l1Utxo) <- partyFunds
            , (txIn, txOut) <- UTxO.toList l1Utxo
            , selectLovelace (txOutValue txOut) >= 2_000_000
            ]
          -- Sort: Alice-first, then by lovelace descending
          ranked =
            sortBy
              (\(p1, _, o1) (p2, _, o2) ->
                case (p1 == FuzzyAlice, p2 == FuzzyAlice) of
                  (True, False) -> LT
                  (False, True) -> GT
                  _ -> compare (selectLovelace (txOutValue o2)) (selectLovelace (txOutValue o1)))
              allEligible
      case ranked of
        [] -> putStrLn "  Seed: no parties have L1 funds >= 2 ADA, skipping"
        (seeder, txIn, txOut) : _ -> do
          cardanoL1Utxo <- queryUTxOFor connectInfo QueryTip (cardanoVkFor env seeder)
          let hasCardanoBuffer = sum [selectLovelace (txOutValue o) | (_, o) <- UTxO.toList cardanoL1Utxo] >= 3_000_000
          if not hasCardanoBuffer
            then putStrLn $ "  Seed: " <> show seeder <> " cardano wallet too small for fee (< 3 ADA), skipping"
            else do
              let seedEligible = UTxO.fromList [(txIn, txOut)]
              commitTx <- retryOnHttpError 5 $
                requestCommitTx (clientFor env seeder) seedEligible
                  <&> \tx -> withSecret (fundsSkFor env seeder) (`signTx` tx)
              runDirectBackend (envDirectOpts env) (submitTransaction commitTx)
              logAction ctx seeder DepositAction
              modifyIORef' (ctxPendingDepositsRef ctx) (+ 1)
              putStrLn $ "  Seed deposit submitted (" <> show seeder <> "), waiting for CommitFinalized..."
              v <- waitMatch 120 (clientFor env seeder) $ \msg ->
                msg ^? key "tag" . _String >>= \case
                  "CommitFinalized" -> Just msg
                  _ -> Nothing
              logEvent ctx v
              putStrLn "  Seed deposit confirmed \x2014 L2 funded"

doDepositFor :: FuzzyParty -> FuzzyEnv -> RoundCtx -> IO ()
doDepositFor party env ctx = do
  let connectInfo = localNodeConnectInfo demoNetworkId (nodeSocket (envDirectOpts env))
  l1Utxo <- queryUTxOFor connectInfo QueryTip (fundsVkFor env party)
  let allList = UTxO.toList l1Utxo
  let eligible = filter (\(_, o) -> selectLovelace (txOutValue o) >= 2_000_000) allList
  case eligible of
    [] -> putStrLn $ "  Deposit: " <> show party <> " has no L1 funds >= 2 ADA, skipping"
    _ -> do
      let (txIn, txOut) = maximumBy (\(_, a) (_, b) -> compare (selectLovelace (txOutValue a)) (selectLovelace (txOutValue b))) eligible
      -- The Hydra node pays commit tx fees from the party's CARDANO KEY wallet.
      -- Need >= 3 ADA so after paying the commit fee (~0.7 ADA) AND a later
      -- close fee (~0.7 ADA), the change output stays above the 849k minimum.
      cardanoL1Utxo <- queryUTxOFor connectInfo QueryTip (cardanoVkFor env party)
      let hasCardanoBuffer = sum [selectLovelace (txOutValue o) | (_, o) <- UTxO.toList cardanoL1Utxo] >= 3_000_000
      if not hasCardanoBuffer
        then putStrLn $ "  Deposit: " <> show party <> " cardano wallet too small for fee (< 3 ADA), skipping"
        else do
          commitTx <-
            requestCommitTx (clientFor env party) (UTxO.fromList [(txIn, txOut)])
              <&> \tx -> withSecret (fundsSkFor env party) (`signTx` tx)
          runDirectBackend (envDirectOpts env) (submitTransaction commitTx)
          logAction ctx party DepositAction
          modifyIORef' (ctxPendingDepositsRef ctx) (+ 1)
          putStrLn $ "  Deposit submitted (" <> show party <> "), continuing without waiting for CommitFinalized"

doDeposit :: FuzzyEnv -> RoundCtx -> IO ()
doDeposit env ctx = do
  party <- pickRandom (ctxRngRef ctx) [FuzzyAlice, FuzzyBob, FuzzyCarol]
  doDepositFor party env ctx

doWaitExpire :: FuzzyEnv -> RoundCtx -> FuzzyOptions -> IO ()
doWaitExpire env ctx opts = do
  party <- pickRandom (ctxRngRef ctx) [FuzzyAlice, FuzzyBob, FuzzyCarol]
  let connectInfo = localNodeConnectInfo demoNetworkId (nodeSocket (envDirectOpts env))
  l1Utxo <- queryUTxOFor connectInfo QueryTip (fundsVkFor env party)
  let allList = UTxO.toList l1Utxo
  let eligible = filter (\(_, o) -> selectLovelace (txOutValue o) >= 2_000_000) allList
  case eligible of
    [] -> putStrLn $ "  WaitExpire: " <> show party <> " has no L1 funds >= 2 ADA, skipping"
    _ -> do
      let (txIn, txOut) = maximumBy (\(_, a) (_, b) -> compare (selectLovelace (txOutValue a)) (selectLovelace (txOutValue b))) eligible
      cardanoL1Utxo <- queryUTxOFor connectInfo QueryTip (cardanoVkFor env party)
      let hasCardanoBuffer = sum [selectLovelace (txOutValue o) | (_, o) <- UTxO.toList cardanoL1Utxo] >= 3_000_000
      if not hasCardanoBuffer
        then putStrLn $ "  WaitExpire: " <> show party <> " cardano wallet too small for fee (< 3 ADA), skipping"
        else do
          commitTx <-
            requestCommitTx (clientFor env party) (UTxO.fromList [(txIn, txOut)])
              <&> \tx -> withSecret (fundsSkFor env party) (`signTx` tx)
          let depositId = getTxId (getTxBody commitTx)
          runDirectBackend (envDirectOpts env) (submitTransaction commitTx)
          logAction ctx party WaitExpireAction
          modifyIORef' (ctxPendingDepositsRef ctx) (+ 1)
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
  startParty <- pickRandom (ctxRngRef ctx) [FuzzyAlice, FuzzyBob, FuzzyCarol]
  l2Utxo <- getSnapshotUTxO (clientFor env startParty)
  let rotatedParties = take 3 $ dropWhile (/= startParty) (cycle [FuzzyAlice, FuzzyBob, FuzzyCarol])
  claimUtxoFrom env ctx l2Utxo rotatedParties >>= \case
    Nothing -> putStrLn "  Decommit: no spendable L2 UTxO, skipping"
    Just (party, _txIn, txOut) -> do
      let selfAddr = mkVkAddress demoNetworkId (fundsVkFor env party)
      case mkSimpleTx (_txIn, txOut) (selfAddr, txOutValue txOut) (fundsSkFor env party) of
        Left err -> do
          atomicModifyIORef' (ctxInFlightRef ctx) (\s -> (Set.delete _txIn s, ()))
          putStrLn $ "  Decommit: build failed: " <> show err
        Right tx -> do
          let decommitTxId = toString (serialiseToRawBytesHexText (getTxId (getTxBody tx)))
          postResult <- try (postDecommit (clientFor env party) tx) :: IO (Either SomeException ())
          case postResult of
            Left ex | any (`isInfixOf` map toLower (show ex)) ["httpexception", "statuscodeexception", "400", "already"] -> do
              atomicModifyIORef' (ctxInFlightRef ctx) (\s -> (Set.delete _txIn s, ()))
              putStrLn "  Decommit: request rejected (node busy / decommit already in flight), skipping"
            Left ex -> do
              atomicModifyIORef' (ctxInFlightRef ctx) (\s -> (Set.delete _txIn s, ()))
              throwIO ex
            Right () -> do
              logAction ctx party DecommitAction
              putStrLn $ "  Decommit submitted (" <> show party <> "), awaiting DecommitApproved..."
              -- Stage 1: wait for DecommitApproved with OUR txId (drains stale WS buffer events
              -- from earlier decommits). DecommitApproved.decommitTxId is the L2 tx hash.
              -- Also catch DecommitInvalid early (rejection before approval).
              approved <- waitMatch (fuzzyStuckTimeout opts + 60) (clientFor env party) $ \msg ->
                msg ^? key "tag" . _String >>= \case
                  "DecommitApproved" ->
                    case msg ^? key "decommitTxId" . _String of
                      Just txId | toString txId == decommitTxId -> Just True
                      _ -> Nothing
                  "DecommitInvalid" ->
                    case msg ^? key "decommitTx" . key "txId" . _String of
                      Just txId | toString txId == decommitTxId -> Just False
                      _ -> Nothing
                  _ -> Nothing
              if not approved
                then do
                  putStrLn "  Decommit rejected (DecommitInvalid), skipping"
                  logEvent ctx (Aeson.object ["tag" .= ("DecommitInvalid" :: Text)])
                else do
                  putStrLn "  Decommit approved, awaiting DecommitFinalized (L1 confirmation)..."
                  -- Stage 2: wait for DecommitFinalized. Since only one decommit can be in-flight
                  -- at a time, any DecommitFinalized arriving after stage 1 is ours.
                  -- The node retries decrementingSnapshot on PostTxOnChainFailed (Bug 2 race
                  -- condition with concurrent deposits), so allow up to 600s.
                  v <- waitMatch (fuzzyStuckTimeout opts + 540) (clientFor env party) $ \msg ->
                    msg ^? key "tag" . _String >>= \case
                      "DecommitFinalized" -> Just msg
                      _ -> Nothing
                  putStrLn "  Decommit finalized"
                  logEvent ctx v

-- | Submit a decommit without waiting for DecommitFinalized, leaving it
-- in-flight when the head closes. Used by ForceCloseAction to exercise the
-- close-with-pending-decommit path.
doDecommitAsync :: FuzzyEnv -> RoundCtx -> IO ()
doDecommitAsync env ctx = do
  startParty <- pickRandom (ctxRngRef ctx) [FuzzyAlice, FuzzyBob, FuzzyCarol]
  l2Utxo <- getSnapshotUTxO (clientFor env startParty)
  let rotatedParties = take 3 $ dropWhile (/= startParty) (cycle [FuzzyAlice, FuzzyBob, FuzzyCarol])
  claimUtxoFrom env ctx l2Utxo rotatedParties >>= \case
    Nothing -> putStrLn "  DecommitAsync: no spendable L2 UTxO, skipping"
    Just (party, txIn, txOut) -> do
      let selfAddr = mkVkAddress demoNetworkId (fundsVkFor env party)
      case mkSimpleTx (txIn, txOut) (selfAddr, txOutValue txOut) (fundsSkFor env party) of
        Left err -> do
          atomicModifyIORef' (ctxInFlightRef ctx) (\s -> (Set.delete txIn s, ()))
          putStrLn $ "  DecommitAsync: build failed: " <> show err
        Right tx -> do
          postDecommit (clientFor env party) tx
          logAction ctx party DecommitAction
          putStrLn $ "  DecommitAsync: submitted (" <> show party <> "), closing without waiting for finalization"

-- | Atomically claim a spendable UTxO not already in-flight (Alice-first order).
claimUtxo ::
  FuzzyEnv ->
  RoundCtx ->
  UTxO ->
  IO (Maybe (FuzzyParty, TxIn, TxOut CAPI.CtxUTxO))
claimUtxo env ctx utxo = claimUtxoFrom env ctx utxo [FuzzyAlice, FuzzyBob, FuzzyCarol]

-- | Like 'claimUtxo' but tries parties in the given order.
claimUtxoFrom ::
  FuzzyEnv ->
  RoundCtx ->
  UTxO ->
  [FuzzyParty] ->
  IO (Maybe (FuzzyParty, TxIn, TxOut CAPI.CtxUTxO))
claimUtxoFrom env ctx utxo parties = do
  let candidates =
        [ (party, txIn, txOut)
        | party <- parties
        , let addr = mkVkAddress demoNetworkId (fundsVkFor env party)
        , (txIn, txOut) <- UTxO.toList utxo
        , txOutAddress txOut == addr
        , selectLovelace (txOutValue txOut) >= 2_000_000
        ]
  atomicModifyIORef' (ctxInFlightRef ctx) $ \inFlight ->
    case filter (\(_, txIn, _) -> txIn `Set.notMember` inFlight) candidates of
      [] -> (inFlight, Nothing)
      (party, txIn, txOut) : _ -> (Set.insert txIn inFlight, Just (party, txIn, txOut))

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
    , selectLovelace (txOutValue txOut) >= 2_000_000
    ]

nextParty :: FuzzyParty -> FuzzyParty
nextParty FuzzyAlice = FuzzyBob
nextParty FuzzyBob = FuzzyCarol
nextParty FuzzyCarol = FuzzyAlice

-- | Decommit all spendable L2 UTxOs sequentially, keeping the LARGEST as the
-- next-round seed. This prevents deposit→decommit L1 fragmentation that causes
-- BabbageOutputTooSmallUTxO errors when the Hydra node picks a tiny fee input.
doDrainAll :: FuzzyEnv -> RoundCtx -> FuzzyOptions -> IO ()
doDrainAll env ctx opts = do
  writeIORef (ctxInFlightRef ctx) mempty
  -- Reserve the largest claimable L2 UTxO so claimUtxo skips it.
  seedParty <- pickRandom (ctxRngRef ctx) [FuzzyAlice, FuzzyBob, FuzzyCarol]
  l2Snapshot <- getSnapshotUTxO (clientFor env seedParty)
  let claimable =
        [ (txIn, txOut)
        | party <- [FuzzyAlice, FuzzyBob, FuzzyCarol]
        , let addr = mkVkAddress demoNetworkId (fundsVkFor env party)
        , (txIn, txOut) <- UTxO.toList l2Snapshot
        , txOutAddress txOut == addr
        , selectLovelace (txOutValue txOut) >= 2_000_000
        ]
  let mKeeper =
        case claimable of
          [] -> Nothing
          _ ->
            Just $
              fst $
                maximumBy
                  (\(_, a) (_, b) -> compare (selectLovelace (txOutValue a)) (selectLovelace (txOutValue b)))
                  claimable
  forM_ mKeeper $ \keeperTxIn ->
    atomicModifyIORef' (ctxInFlightRef ctx) (\s -> (Set.insert keeperTxIn s, ()))
  putStrLn "  DrainAll: draining L2 UTxOs (keeping largest for next round)..."
  go (0 :: Int)
  forM_ mKeeper $ \keeperTxIn ->
    atomicModifyIORef' (ctxInFlightRef ctx) (\s -> (Set.delete keeperTxIn s, ()))
 where
  go count = do
    startParty <- pickRandom (ctxRngRef ctx) [FuzzyAlice, FuzzyBob, FuzzyCarol]
    l2Utxo <- getSnapshotUTxO (clientFor env startParty)
    claimUtxo env ctx l2Utxo >>= \case
      Nothing -> putStrLn $ "  DrainAll: done, decommitted " <> show count <> " UTxO(s)"
      Just (party, txIn, txOut) -> do
        let selfAddr = mkVkAddress demoNetworkId (fundsVkFor env party)
        case mkSimpleTx (txIn, txOut) (selfAddr, txOutValue txOut) (fundsSkFor env party) of
          Left err -> do
            atomicModifyIORef' (ctxInFlightRef ctx) (\s -> (Set.delete txIn s, ()))
            putStrLn $ "  DrainAll: build failed: " <> show err
          Right tx -> do
            let decommitTxId = toString (serialiseToRawBytesHexText (getTxId (getTxBody tx)))
            retryOnHttpError 5 (postDecommit (clientFor env party) tx)
            logAction ctx party DecommitAction
            putStrLn $ "  DrainAll: decommit " <> show (count + 1) <> " submitted (" <> show party <> ")..."
            -- Stage 1: wait for DecommitApproved with OUR txId (drains stale WS buffer).
            approved <- waitMatch (fuzzyStuckTimeout opts + 60) (clientFor env party) $ \msg ->
              msg ^? key "tag" . _String >>= \case
                "DecommitApproved" ->
                  case msg ^? key "decommitTxId" . _String of
                    Just txId | toString txId == decommitTxId -> Just True
                    _ -> Nothing
                "DecommitInvalid" ->
                  case msg ^? key "decommitTx" . key "txId" . _String of
                    Just txId | toString txId == decommitTxId -> Just False
                    _ -> Nothing
                _ -> Nothing
            if not approved
              then do
                atomicModifyIORef' (ctxInFlightRef ctx) (\s -> (Set.delete txIn s, ()))
                putStrLn $ "  DrainAll: decommit " <> show (count + 1) <> " rejected (DecommitInvalid), stopping drain"
              else do
                -- Stage 2: wait for DecommitFinalized — only one in-flight at a time, so
                -- any DecommitFinalized after stage 1 is ours.
                v <- waitMatch (fuzzyStuckTimeout opts + 540) (clientFor env party) $ \msg ->
                  msg ^? key "tag" . _String >>= \case
                    "DecommitFinalized" -> Just msg
                    _ -> Nothing
                logEvent ctx v
                go (count + 1)

-- | Sum of all lovelace owned by the three fuzzy parties on L1.
totalL1Lovelace :: FuzzyEnv -> IO Integer
totalL1Lovelace env = do
  let connectInfo = localNodeConnectInfo demoNetworkId (nodeSocket (envDirectOpts env))
  fmap sum $
    forM [FuzzyAlice, FuzzyBob, FuzzyCarol] $ \party ->
      fromIntegral . UTxO.totalLovelace
        <$> queryUTxOFor connectInfo QueryTip (fundsVkFor env party)

-- * Close phase

closePhase :: FuzzyEnv -> FuzzyOptions -> RoundCtx -> IO ()
closePhase env opts ctx = do
  putStrLn "Close phase: sending Close..."
  -- Returns True if ReadyToFanout was already consumed (contestation already passed)
  alreadyReady <- sendCloseAndWait env ctx 5

  -- 30% chance to contest from Bob or Carol, with cascade (only if we just closed)
  unless alreadyReady $ do
    rng <- readIORef (ctxRngRef ctx)
    let (n, rng') = randomR (0 :: Int, 9) rng
    writeIORef (ctxRngRef ctx) rng'
    when (n < 3) $ do
      contestant <- pickRandom (ctxRngRef ctx) [FuzzyBob, FuzzyCarol]
      putStrLn $ "Contesting from " <> show contestant <> "..."
      send (clientFor env contestant) (input "Contest" [])
      logAction ctx contestant ContestAction
      drainEvents 5 env ctx
      rng2 <- readIORef (ctxRngRef ctx)
      let (n2, rng2') = randomR (0 :: Int, 9) rng2
      writeIORef (ctxRngRef ctx) rng2'
      when (n2 < 3) $ do
        let other = if contestant == FuzzyBob then FuzzyCarol else FuzzyBob
        putStrLn $ "Cascade contest from " <> show other <> "..."
        send (clientFor env other) (input "Contest" [])
        logAction ctx other ContestAction
        drainEvents 5 env ctx

  unless alreadyReady $ do
    let waitBudget = 120 + floor (fuzzyStuckTimeout opts) :: Int
    putStrLn $ "Waiting for ReadyToFanout (up to " <> show waitBudget <> "s)..."
    _ <- waitMatch (fromIntegral waitBudget) (envAliceClient env) $ \v ->
      guard (v ^? key "tag" . _String == Just "ReadyToFanout") $> v
    putStrLn "ReadyToFanout."

-- Returns True if ReadyToFanout was already seen (contestation period passed while waiting).
sendCloseAndWait :: FuzzyEnv -> RoundCtx -> Int -> IO Bool
sendCloseAndWait env ctx retriesLeft = do
  closeParty <- pickRandom (ctxRngRef ctx) [FuzzyAlice, FuzzyBob, FuzzyCarol]
  send (clientFor env closeParty) (input "Close" [])
  logAction ctx closeParty CloseAction
  -- Listen on Alice's client (always active); accept both HeadIsClosed and
  -- ReadyToFanout so we don't miss the window when the contestation period
  -- (3 s) elapses before the next waitMatch call.
  -- 300s gives generous headroom for slow L1 and large WS event backlogs.
  result <- try (waitMatch 300 (envAliceClient env) $ \v ->
    v ^? key "tag" . _String >>= \case
      "HeadIsClosed" -> Just False
      "ReadyToFanout" -> Just True
      _ -> Nothing) :: IO (Either SomeException Bool)
  case result of
    Right alreadyReady -> do
      putStrLn $ if alreadyReady then "ReadyToFanout (contestation passed)." else "HeadIsClosed."
      pure alreadyReady
    Left _
      | retriesLeft > 0 -> do
          putStrLn $ "Close not confirmed in 300s, retrying (" <> show retriesLeft <> " retries left)..."
          threadDelay 5_000_000
          sendCloseAndWait env ctx (retriesLeft - 1)
      | otherwise ->
          throwIO $ ErrorCall "Close tx failed after all retries"

-- * Fanout phase

-- | Maximum lovelace loss attributable to L1 fees across a full round.
maxFeeLoss :: Integer
maxFeeLoss = 10_000_000

fanoutPhase :: FuzzyEnv -> UTxO -> RoundCtx -> IO ()
fanoutPhase env snapshotUtxo ctx = do
  let snapshotLovelace = UTxO.totalLovelace snapshotUtxo
  sendFanoutAndWait env ctx 3

  modifyIORef' (ctxStatsRef ctx) $ \s -> s{utxoCheckPassed = True}
  putStrLn $ "Fanout complete (last snapshot had " <> show snapshotLovelace <> " lovelace)"

  stats <- readIORef (ctxStatsRef ctx)
  let l1Before = l1LovelaceBefore stats
  pending <- readIORef (ctxPendingDepositsRef ctx)
  when (l1Before > 0 && pending > 0) $
    putStrLn $ "  L1 balance check skipped (" <> show pending <> " deposit(s) still pending in deposit contract)"
  when (l1Before > 0 && pending == 0) $ do
    l1After <- totalL1Lovelace env
    let loss = l1Before - l1After
        balanceOk = loss <= maxFeeLoss
    modifyIORef' (ctxStatsRef ctx) $ \s -> s{l1LovelaceAfter = l1After, l1BalanceOk = balanceOk}
    putStrLn $
      "L1 balance after fanout: "
        <> show l1After
        <> " lovelace (delta="
        <> show (l1After - l1Before)
        <> ")"
    unless balanceOk $
      throwIO $
        ErrorCall $
          "L1ValueLoss:" <> show loss <> " lovelace (max allowed " <> show maxFeeLoss <> ")"

sendFanoutAndWait :: FuzzyEnv -> RoundCtx -> Int -> IO ()
sendFanoutAndWait env ctx retriesLeft = do
  fanoutParty <- pickRandom (ctxRngRef ctx) [FuzzyAlice, FuzzyBob, FuzzyCarol]
  when (retriesLeft == 3) $ logAction ctx fanoutParty FanoutAction
  send (clientFor env fanoutParty) (input "Fanout" [])
  deadline <- addUTCTime 60 <$> getCurrentTime
  finalized <- pollUntilFinalized fanoutParty deadline
  if finalized
    then putStrLn "HeadIsFinalized."
    else
      if retriesLeft > 0
        then do
          putStrLn $ "Fanout not confirmed in 60s, retrying (" <> show retriesLeft <> " left)..."
          sendFanoutAndWait env ctx (retriesLeft - 1)
        else throwIO $ ErrorCall "FanoutFailed: not confirmed after all retries"
 where
  pollUntilFinalized :: FuzzyParty -> UTCTime -> IO Bool
  pollUntilFinalized fanoutParty deadline = do
    now <- getCurrentTime
    let remaining = diffUTCTime deadline now
    if remaining <= 0
      then pure False
      else
        tryGetEvent (min remaining 5.0) (clientFor env fanoutParty) >>= \case
          Nothing -> pollUntilFinalized fanoutParty deadline
          Just v -> do
            logEvent ctx v
            case v ^? key "tag" . _String of
              Just "HeadIsFinalized" -> pure True
              Just "PostTxOnChainFailed" -> do
                let err = show <$> (v ^? key "postTxError" :: Maybe Value)
                putStrLn $ "  Fanout: on-chain tx failed: " <> fromMaybe "?" err
                pollUntilFinalized fanoutParty deadline
              _ -> pollUntilFinalized fanoutParty deadline
