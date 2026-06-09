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
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

failureLabel :: FailureType -> String
failureLabel (StuckSnapshot s) = "StuckSnapshot after " <> show s <> "s"
failureLabel UTxOMismatch = "UTxOMismatch"
failureLabel (NodeCrash r) = "NodeCrash: " <> r

data ActionTag
  = NewTxAction
  | DepositAction
  | DecommitAction
  | WaitExpireAction
  | CloseAction
  | ContestAction
  | FanoutAction
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

actionTagLabel :: ActionTag -> String
actionTagLabel = \case
  NewTxAction -> "NewTx"
  DepositAction -> "Deposit"
  DecommitAction -> "Decommit"
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
  , roundFailure :: Maybe FailureType
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
    , roundFailure = Nothing
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
    [ "hydra-fuzzy run report — " <> maybe "n/a" (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" . roundStart) (listToMaybe stats)
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
        , "  Avg latency       : " <> renderLatency (snapshotLatencies rs)
        , "  Deposits finalized: " <> show (depositsFinalized rs)
        , "  Deposits expired  : " <> show (depositsExpired rs)
        , "  Decommits finalized:" <> show (decommitsFinalized rs)
        , ""
        ]

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

  renderOverall :: [RoundStats] -> [String]
  renderOverall ss =
    let completed = length $ filter (isNothing . roundFailure) ss
        total = sum [sum (Map.elems (actionCounts rs)) | rs <- ss]
        bugs = length $ filter (isJust . roundFailure) ss
     in [ "OVERALL"
        , "  Rounds completed: " <> show completed <> "/" <> show (length ss)
        , "  Total actions   : " <> show total
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
renderFailureReport FailureReport{frRound, frFailureType, frActionLog, frServerOutputLog, frPersistenceLogPaths} =
  intercalate "\n" $
    [ "FAILURE REPORT — Round " <> show frRound
    , replicate 40 '='
    , ""
    , "Failure  : " <> failureLabel frFailureType
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
