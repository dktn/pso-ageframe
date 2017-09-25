{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}

module Config
    ( loadConfig
    , Config(..)
    ) where

import           GHC.Generics
import           Data.Text         (Text)
import           System.FilePath   (FilePath)

import           Data.Yaml


data Config = Config { input      :: Text
                     , iterations :: Maybe Integer
                     } deriving (Eq, Show, Generic, FromJSON)

loadConfig :: FilePath -> IO (Either String Config)
loadConfig = decodeYaml

decodeYaml :: FromJSON a => FilePath -> IO (Either String a)
decodeYaml file = do
    result <- decodeFileEither file
    return $ either (Left . errToString) Right result
    where
        errToString err = file ++ case err of
            AesonException e                          -> ": " ++ e
            InvalidYaml (Just (YamlException s))      -> ": " ++ s
            InvalidYaml (Just YamlParseException{..}) -> ":"  ++ show yamlLine
                                                      ++ ":"  ++ show yamlColumn
                                                      ++ ": " ++ yamlProblem
                                                      ++ " "  ++ yamlContext
                where YamlMark{..} = yamlProblemMark
            _                                         -> ": " ++ show err
