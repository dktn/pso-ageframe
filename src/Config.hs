{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}

module Config
    ( loadConfig
    , Config(..)
    ) where

import           Protolude
import           Data.Text (Text)
import           GHC.Word (Word32)

import           Data.Yaml


data Config = Config { input      :: Text
                     , seed       :: Word32
                     , dimension  :: Int
                     , swarmSize  :: Int
                     , iterations :: Maybe Integer
                     } deriving (Eq, Show, Generic, FromJSON)

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
