{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Citeproc (
    run
)   where

import           System.Exit        (exitFailure)
import qualified Data.ByteString.Char8 as BS
import           Control.Monad      (forM_)
import           Control.Applicative

import Data.Semigroup
import System.FilePath
import System.Directory

import Control.Error
import Control.Monad.IO.Class

import Data.Tagged

import Data.Aeson (FromJSON, Value)
import Data.Aeson.AutoType.Alternative

import qualified Data.Aeson as A

import qualified Data.Yaml as Y
import qualified Data.Yaml.Pretty as Y

import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Citeproc.Auto

outputYaml :: [TopLevelElt] -> TopLevel
outputYaml topLevelYaml = AltLeft TopLevelYaml{..}

parseYaml :: forall m . MonadIO m => FilePath -> m TopLevel
parseYaml = parseMarkup Y.decode

parseJson :: forall m . MonadIO m => FilePath -> m TopLevel
parseJson = parseMarkup A.decodeStrict'

parseYorJ :: forall m . MonadIO m => FilePath -> m TopLevel
parseYorJ = parseMarkup $ (<|>) <$> Y.decode <*> A.decodeStrict'

parseMarkup
        :: forall m . MonadIO m
        => (forall a . FromJSON a => BS.ByteString -> Maybe a)
        -> FilePath -> m TopLevel
parseMarkup decode_ filename = do
        input <- liftIO . BS.readFile $ filename
        case decode_ input of
            Nothing -> fatal $ case (decode_ input :: Maybe Value) of
                Nothing -> "Invalid JSON file: "     <> T.pack filename
                Just _  -> "Mismatched JSON value from file: " <> T.pack filename
            Just r  -> return (r :: TopLevel)
    where
        fatal :: Text -> m a
        fatal msg = liftIO $ do
            errLn msg
            exitFailure

run :: MonadIO m
    => Either (Tagged "doi" String) (Tagged "identifier" String)
    -> Maybe (Tagged "write-to-file" FilePath)
    -> Tagged "standalone" Bool
    -> [Tagged "filename" FilePath]
    -> m ()
run ident writeToFile standalone filenames =
        forM_ filenames $ \(Tagged f) -> runMaybeT $ do
            library <- parseYorJ f
            match <- hoistMaybe . headMay . filter searchFilter . alt topLevelYaml id $ library
            let rewrap = if untag standalone then ("---\n" <>) . (<> "\n---\n") else (<> "\n") . B8.drop 12 -- drop the 'references:\n' leader
            let refText = rewrap . Y.encodePretty (orderingTopLevelElt `Y.setConfCompare` Y.defConfig) . outputYaml $ [match]
            case writeToFile of
                Just (Tagged x) -> do
                    doi <- hoistMaybe . getDOI $ match
                    let refFile = x </> T.unpack doi <.> "yaml"
                    liftIO . createDirectoryIfMissing True . takeDirectory $ refFile
                    liftIO . (`BS.writeFile` refText) $ refFile
                    liftIO . errLn . T.unwords $ ["Created", T.pack refFile, "for", doi]
                Nothing -> liftIO . BS.putStr $ refText
    where
        searchFilter = case ident of
            Left (Tagged search)  -> maybe False (T.pack search ==) . getDOI
            Right (Tagged search) -> (T.pack search ==) . topLevelEltId


