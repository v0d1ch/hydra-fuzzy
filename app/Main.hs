{-# LANGUAGE ApplicativeDo #-}

module Main where

import Hydra.Prelude

import FuzzyActions (FuzzyOptions (..), defaultFuzzyOptions, runFuzzy)
import Hydra.Network (Host (..))
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
  showDefault,
  strOption,
  switch,
  value,
 )

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  execParser optionsInfo >>= runFuzzy

optionsInfo :: ParserInfo FuzzyOptions
optionsInfo =
  info
    (helper <*> parseOptions)
    ( fullDesc
        <> progDesc "Random action fuzzer for a running Hydra demo"
        <> header "hydra-fuzzy - stress test and fuzz Hydra Head operations"
    )

parseOptions :: Parser FuzzyOptions
parseOptions = do
  fuzzyRounds <-
    option auto $
      long "rounds"
        <> short 'r'
        <> metavar "N"
        <> value (fuzzyRounds defaultFuzzyOptions)
        <> showDefault
        <> help "Number of rounds (Init→Open→Close→Fanout)"
  fuzzyTxsPerRound <-
    option auto $
      long "txs-per-round"
        <> short 'n'
        <> metavar "N"
        <> value (fuzzyTxsPerRound defaultFuzzyOptions)
        <> showDefault
        <> help "Max random actions in Open phase per round (sequential mode)"
  fuzzyStress <-
    switch $
      long "stress"
        <> help "Enable stress mode: expand UTxOs then flood NewTx for --flood-duration seconds"
  fuzzyParallelism <-
    option auto $
      long "parallelism"
        <> short 'p'
        <> metavar "N"
        <> value (fuzzyParallelism defaultFuzzyOptions)
        <> showDefault
        <> help "Parallel sender threads in stress mode"
  fuzzyFloodDuration <-
    fmap (fromIntegral @Int) $
      option auto $
        long "flood-duration"
          <> metavar "SECS"
          <> value (floor (fuzzyFloodDuration defaultFuzzyOptions) :: Int)
          <> showDefault
          <> help "Seconds to flood txs per round in stress mode"
  fuzzyExpandUtxo <-
    option auto $
      long "expand-utxo"
        <> metavar "N"
        <> value (fuzzyExpandUtxo defaultFuzzyOptions)
        <> showDefault
        <> help "Target UTxO count per party before stress flood (expansion phase)"
  fuzzyStuckTimeout <-
    fmap (fromIntegral @Int) $
      option auto $
        long "stuck-timeout"
          <> metavar "SECS"
          <> value (floor (fuzzyStuckTimeout defaultFuzzyOptions) :: Int)
          <> showDefault
          <> help "Seconds without SnapshotConfirmed before declaring stuck"
  fuzzySeed <-
    option auto $
      long "seed"
        <> metavar "N"
        <> value (fuzzySeed defaultFuzzyOptions)
        <> showDefault
        <> help "Random seed for reproducible runs"
  fuzzyOutputDir <-
    strOption $
      long "output"
        <> short 'o'
        <> metavar "DIR"
        <> value (fuzzyOutputDir defaultFuzzyOptions)
        <> showDefault
        <> help "Directory for run reports"
  alicePort <-
    option auto $
      long "alice-port"
        <> metavar "PORT"
        <> value (4001 :: Int)
        <> showDefault
        <> help "Alice's hydra-node API port"
  bobPort <-
    option auto $
      long "bob-port"
        <> metavar "PORT"
        <> value (4002 :: Int)
        <> showDefault
        <> help "Bob's hydra-node API port"
  carolPort <-
    option auto $
      long "carol-port"
        <> metavar "PORT"
        <> value (4003 :: Int)
        <> showDefault
        <> help "Carol's hydra-node API port"
  fuzzySocketPath <-
    strOption $
      long "socket"
        <> metavar "PATH"
        <> value (fuzzySocketPath defaultFuzzyOptions)
        <> showDefault
        <> help "Cardano-node socket path"
  fuzzyDepositExpireWait <-
    fmap (fromIntegral @Int) $
      option auto $
        long "deposit-expire-wait"
          <> metavar "SECS"
          <> value (floor (fuzzyDepositExpireWait defaultFuzzyOptions) :: Int)
          <> showDefault
          <> help "Seconds to wait for a deposit to expire in WaitExpire action"
  pure
    FuzzyOptions
      { fuzzyRounds
      , fuzzyTxsPerRound
      , fuzzyStress
      , fuzzyParallelism
      , fuzzyFloodDuration
      , fuzzyExpandUtxo
      , fuzzyStuckTimeout
      , fuzzySeed
      , fuzzyOutputDir
      , fuzzyAliceHost = Host "127.0.0.1" (fromIntegral alicePort)
      , fuzzyBobHost = Host "127.0.0.1" (fromIntegral bobPort)
      , fuzzyCarolHost = Host "127.0.0.1" (fromIntegral carolPort)
      , fuzzySocketPath
      , fuzzyDepositExpireWait
      }
