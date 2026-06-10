{-# LANGUAGE DuplicateRecordFields #-}

module FuzzyReport where

import Hydra.Prelude

import Control.Lens ((^?))
import Data.Aeson (Value)
import Data.Aeson.Lens (key, _String)
import Data.List (maximum, minimum)
import Data.Map.Strict qualified as Map
import Data.Time (defaultTimeLocale, formatTime)
import System.FilePath ((</>))

data FailureType
  = StuckSnapshot {stuckSeconds :: Int}
  | UTxOMismatch
  | NodeCrash {crashReason :: String}
  | L1ValueLoss {lovelaceLost :: Integer}
  | FanoutFailed {fanoutError :: String}
  | TransientError {transientReason :: String}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

failureLabel :: FailureType -> String
failureLabel (StuckSnapshot s) = "StuckSnapshot after " <> show s <> "s"
failureLabel UTxOMismatch = "UTxOMismatch"
failureLabel (NodeCrash r) = "NodeCrash: " <> r
failureLabel (L1ValueLoss n) = "L1ValueLoss: " <> show n <> " lovelace"
failureLabel (FanoutFailed e) = "FanoutFailed: " <> e
failureLabel (TransientError r) = "TransientError: " <> take 120 r

data ActionTag
  = NewTxAction
  | MultiSendAction
  | DepositAction
  | DecommitAction
  | DrainAllAction
  | WaitExpireAction
  | CloseAction
  | ContestAction
  | FanoutAction
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

actionTagLabel :: ActionTag -> String
actionTagLabel = \case
  NewTxAction -> "NewTx"
  MultiSendAction -> "MultiSend"
  DepositAction -> "Deposit"
  DecommitAction -> "Decommit"
  DrainAllAction -> "DrainAll"
  WaitExpireAction -> "WaitExpire"
  CloseAction -> "Close"
  ContestAction -> "Contest"
  FanoutAction -> "Fanout"

data ActionEntry = ActionEntry
  { aeTimestamp :: UTCTime
  , aeActorId :: Int
  , aeTag :: ActionTag
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data RoundStats = RoundStats
  { roundNumber :: Int
  , roundStart :: UTCTime
  , roundEnd :: UTCTime
  , actionCounts :: Map ActionTag Int
  , txValidCount :: Int
  , txInvalidCount :: Int
  , snapshotCount :: Int
  , snapshotLatencies :: [NominalDiffTime]
  , depositsFinalized :: Int
  , depositsExpired :: Int
  , decommitsFinalized :: Int
  , utxoCheckPassed :: Bool
  , l1LovelaceBefore :: Integer
  , l1LovelaceAfter :: Integer
  , l1BalanceOk :: Bool
  , roundFailure :: Maybe FailureType
  , roundThroughputTps :: Double
  , floodPeakTps :: Double
  }

emptyRoundStats :: Int -> UTCTime -> RoundStats
emptyRoundStats n t =
  RoundStats
    { roundNumber = n
    , roundStart = t
    , roundEnd = t
    , actionCounts = Map.empty
    , txValidCount = 0
    , txInvalidCount = 0
    , snapshotCount = 0
    , snapshotLatencies = []
    , depositsFinalized = 0
    , depositsExpired = 0
    , decommitsFinalized = 0
    , utxoCheckPassed = False
    , l1LovelaceBefore = 0
    , l1LovelaceAfter = 0
    , l1BalanceOk = False
    , roundFailure = Nothing
    , roundThroughputTps = 0.0
    , floodPeakTps = 0.0
    }

data RunConfig = RunConfig
  { cfgRounds :: Int
  , cfgTxsPerRound :: Int
  , cfgSeed :: Int
  , cfgStuckTimeout :: NominalDiffTime
  }

data FailureReport = FailureReport
  { frRound :: Int
  , frFailureType :: FailureType
  , frRawException :: String
  , frActionLog :: [ActionEntry]
  , frServerOutputLog :: [Value]
  , frPersistenceLogPaths :: Map Text FilePath
  }
  deriving stock (Generic)

formatTimestamp :: UTCTime -> String
formatTimestamp = formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ"

writeRunReport :: FilePath -> RunConfig -> [RoundStats] -> IO FilePath
writeRunReport outputDir config stats = do
  now <- getCurrentTime
  let filename = outputDir </> "hydra-fuzzy-run-" <> formatTimestamp now <> ".md"
  writeFile filename (renderReport config stats)
  pure filename

renderReport :: RunConfig -> [RoundStats] -> String
renderReport RunConfig{cfgRounds, cfgTxsPerRound, cfgSeed, cfgStuckTimeout} stats =
  intercalate "\n" $
    header
      <> concatMap renderRound stats
      <> renderOverall stats
 where
  header :: [String]
  header =
    [ "hydra-fuzzy run report \x2014 " <> maybe "n/a" (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" . roundStart) (listToMaybe stats)
    , replicate 56 '='
    , ""
    , "Config: "
        <> show cfgRounds
        <> " rounds \xd7 "
        <> show cfgTxsPerRound
        <> " txs, seed "
        <> show cfgSeed
        <> ", stuck-timeout "
        <> show (floor cfgStuckTimeout :: Int)
        <> "s"
    , ""
    ]

  renderRound :: RoundStats -> [String]
  renderRound rs =
    let status = case roundFailure rs of
          Nothing -> "\x2713  (PASS \x2014 UTxO " <> (if utxoCheckPassed rs then "verified" else "check skipped") <> ")"
          Just f -> "\x2717  (FAIL \x2014 " <> failureLabel f <> ")"
        dur = floor (diffUTCTime (roundEnd rs) (roundStart rs)) :: Int
        totalActs = sum (Map.elems (actionCounts rs))
        actDetail =
          intercalate
            ", "
            [ actionTagLabel t <> "=" <> show n
            | (t, n) <- Map.toList (actionCounts rs)
            ]
     in [ "Round "
            <> show (roundNumber rs)
            <> "  "
            <> status
        , "  Duration          : " <> show dur <> "s"
        , "  Actions sent      : " <> show totalActs <> " (" <> actDetail <> ")"
        , "  TxValid           : " <> show (txValidCount rs)
        , "  TxInvalid         : " <> show (txInvalidCount rs)
        , "  SnapshotConfirmed : " <> show (snapshotCount rs)
        , "  Throughput        : " <> renderThroughput (roundThroughputTps rs) <> if floodPeakTps rs > 0 then " (flood peak: " <> renderThroughput (floodPeakTps rs) <> ")" else ""
        , "  Avg latency       : " <> renderLatency (snapshotLatencies rs)
        , "  Deposits finalized: " <> show (depositsFinalized rs)
        , "  Deposits expired  : " <> show (depositsExpired rs)
        , "  Decommits finalized:" <> show (decommitsFinalized rs)
        , "  L1 balance        : " <> renderL1Balance rs
        , ""
        ]

  renderThroughput :: Double -> String
  renderThroughput tps
    | tps <= 0 = "n/a"
    | otherwise = show (round tps :: Int) <> " tx/s"

  renderLatency :: [NominalDiffTime] -> String
  renderLatency [] = "n/a"
  renderLatency lats =
    let toMs :: NominalDiffTime -> String
        toMs l = show (floor (realToFrac l * 1000 :: Double) :: Int) <> "ms"
        avg = sum lats / fromIntegral (length lats)
     in toMs avg
          <> " avg (min "
          <> toMs (minimum lats)
          <> ", max "
          <> toMs (maximum lats)
          <> ")"

  renderL1Balance :: RoundStats -> String
  renderL1Balance rs
    | l1LovelaceBefore rs == 0 = "not measured"
    | isJust (roundFailure rs) && l1LovelaceAfter rs == 0 =
        "before="
          <> show (l1LovelaceBefore rs)
          <> " after=n/a (round failed before fanout)"
    | otherwise =
        let before = l1LovelaceBefore rs
            after = l1LovelaceAfter rs
            delta = after - before
            deltaStr = (if delta >= 0 then "+" else "") <> show delta
         in "before="
              <> show before
              <> " after="
              <> show after
              <> " delta="
              <> deltaStr
              <> " lovelace  "
              <> (if l1BalanceOk rs then "OK" else "LOSS")

  renderOverall :: [RoundStats] -> [String]
  renderOverall ss =
    let completed = length $ filter (isNothing . roundFailure) ss
        total = sum [sum (Map.elems (actionCounts rs)) | rs <- ss]
        bugs = length $ filter (isJust . roundFailure) ss
        allTps = [roundThroughputTps s | s <- ss, roundThroughputTps s > 0]
        avgTps = if null allTps then 0.0 else sum allTps / fromIntegral (length allTps)
     in [ "OVERALL"
        , "  Rounds completed: " <> show completed <> "/" <> show (length ss)
        , "  Total actions   : " <> show total
        , "  Avg throughput  : " <> (if avgTps <= 0 then "n/a" else show (round avgTps :: Int) <> " tx/s")
        , "  Bugs found      : " <> show bugs
        ]

actorName :: Int -> String
actorName 1 = "Alice"
actorName 2 = "Bob"
actorName 3 = "Carol"
actorName n = "Party" <> show n

renderServerEvent :: Value -> String
renderServerEvent v =
  "  " <> maybe "?" toString (v ^? key "tag" . _String)

renderActionEntry :: ActionEntry -> String
renderActionEntry ActionEntry{aeTimestamp, aeActorId, aeTag} =
  "  "
    <> formatTime defaultTimeLocale "%H:%M:%S" aeTimestamp
    <> "  "
    <> let a = actorName aeActorId
        in a
            <> replicate (max 0 (6 - length a)) ' '
            <> "  "
            <> actionTagLabel aeTag

renderFailureReport :: FailureReport -> String
renderFailureReport FailureReport{frRound, frFailureType, frRawException, frActionLog, frServerOutputLog, frPersistenceLogPaths} =
  intercalate "\n" $
    [ "FAILURE REPORT \x2014 Round " <> show frRound
    , replicate 40 '='
    , ""
    , "Failure  : " <> failureLabel frFailureType
    , "Exception: " <> take 800 frRawException
    , ""
    , "Action timeline (" <> show (length frActionLog) <> " actions):"
    ]
      <> map renderActionEntry frActionLog
      <> [ ""
         , "Last " <> show (length evts) <> " server events:"
         ]
      <> map renderServerEvent evts
      <> [ ""
         , "Persistence logs:"
         ]
      <> ["  " <> toString k <> ": " <> v | (k, v) <- Map.toList frPersistenceLogPaths]
 where
  evts = take 30 frServerOutputLog

writeFailureReport :: FilePath -> FailureReport -> IO FilePath
writeFailureReport outputDir report = do
  now <- getCurrentTime
  let filename = outputDir </> "hydra-fuzzy-failure-" <> formatTimestamp now <> ".md"
  writeFile filename (renderFailureReport report)
  pure filename
