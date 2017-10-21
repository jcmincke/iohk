
{-# LANGUAGE RecordWildCards #-}
module Config
(
  parseCliArgs
  , makeConfig
  , Config(..)
)
where


import Data.Maybe (fromMaybe)
import Data.Monoid
import Options.Applicative

data Options = MkOptions {
  optHost :: Maybe String
  , optPort :: Maybe String
  , optSubmissionPeriod :: Int
  , optGracePeriod :: Int
  , optWithSeed :: Maybe Int
  , optNbMsgs :: Maybe Int
  , optComReliabilityProb :: Float
  }
  deriving (Show)

optionParser :: Parser Options
optionParser =
  MkOptions
        <$> optional (strOption (
              long "host"
              <> short 'h'
              <> (help "Host address")))
        <*> optional (strOption (
              long "port"
              <> short 'p'
              <> (help "Port")))
        <*> (option auto (
              long "send-for"
              <> value 30
              <> (help "Submission period in seconds")))
        <*> (option auto (
              long "wait-for"
              <> value 5
              <> (help "Grace period in seconds")))
        <*> optional (option auto (
              long "with-seed"
              <> (help "Seed")))
        <*> optional (option auto (
              long "nb-msgs"
              <> (help "Number of messages to generate during the submission period")))
        <*> (option auto (
              long "com-rel"
              <> value 1.0
              <> (help "Reliability of communication [0-1]")))

-- | Parse command line options
parseCliArgs :: IO Options
parseCliArgs = customExecParser (prefs showHelpOnError) (info optionParser fullDesc)

-- | General configuration for an IOHK agent.
data Config = MkConfig {
  cfHost :: String                  -- ^ Post address.
  , cfPort :: String                -- ^ Port number.
  , cfSubmissionPeriod :: Int       -- ^ The submission period in microseconds.
  , cfGracePeriod :: Int            -- ^ The grace period in microseconds.
  , cfSeed :: Int                   -- ^ Seed for the RNG that generates values and times.
  , cfNbMsgs :: Int                 -- ^ Number of message to be sent during the submission period.
  , cfCompactingMaxLen :: Int       -- ^ Maximum number of messages in payload before compaction happens.
  , cfComReliabilityProb :: Float   -- ^ Probability a message is sent correctly.
  }
  deriving (Show)

-- | Create a Config from command line options.
makeConfig :: [Char] -> [Char] -> Int -> Int -> Options -> Config
makeConfig host port seed nbMsgs (MkOptions {..}) =
  MkConfig{..}
  where
  cfHost = fromMaybe host optHost
  cfPort = fromMaybe port optPort
  cfSubmissionPeriod = optSubmissionPeriod * 1000000
  cfGracePeriod = optGracePeriod * 1000000
  cfSeed = fromMaybe seed optWithSeed
  cfNbMsgs = fromMaybe nbMsgs optNbMsgs
  cfComReliabilityProb = optComReliabilityProb
  -- maximum number of messages in a payload before compaction happens
  -- Set it to a fraction of the total number of messages to enable campaction
  cfCompactingMaxLen = maxBound




