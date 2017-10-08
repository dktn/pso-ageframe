{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}

module Config
    ( loadConfig
    , Config(..)
    , epochsDef
    , logEpochsDef
    ) where

import           Protolude
import           Data.Text (Text)
import           GHC.Word (Word32)

import           Data.Yaml


data Config = Config { input      :: Text
                     , seed       :: Maybe Word32
                     , dimension  :: Int
                     , swarmSize  :: Int
                     , epochs     :: Maybe Integer
                     , logEpochs  :: Maybe Integer
                     } deriving (Eq, Show, Generic, FromJSON)

epochsDef :: Config -> Integer
epochsDef = fromMaybe 100 . epochs

logEpochsDef :: Config -> Integer
logEpochsDef = fromMaybe 10 . logEpochs

loadConfig :: Text -> IO (Either Text Config)
loadConfig = decodeYaml

decodeYaml :: FromJSON a => Text -> IO (Either Text a)
decodeYaml file = do
    result <- decodeFileEither $ toS file
    return $ either (Left . errToString) Right result
      where
        errToString :: ParseException -> Text
        errToString err = file <> case err of
            AesonException e                          -> ": " <> toS e
            InvalidYaml (Just (YamlException s))      -> ": " <> toS s
            InvalidYaml (Just YamlParseException{..}) -> ":"  <> show yamlLine
                                                      <> ":"  <> show yamlColumn
                                                      <> ": " <> toS yamlProblem
                                                      <> " "  <> toS yamlContext
              where
                YamlMark{..} = yamlProblemMark
            _                                         -> ": " <> show err
