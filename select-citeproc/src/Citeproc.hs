{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Citeproc (
    main
)   where

import           System.Exit        (exitFailure)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Char8 as BS
import           Control.Monad      (forM_)

import ReadArgs
import System.Directory
import System.FilePath
import Control.Error
import Control.Monad.IO.Class

import Data.Yaml
import Data.Yaml.Pretty
import Data.Text (pack)

import Citeproc.Auto

parseYaml :: forall m . MonadIO m => FilePath -> m TopLevel
parseYaml filename = do
        input <- liftIO . BS.readFile $ filename
        case decode input of
            Nothing -> fatal $ case (decode input :: Maybe Value) of
                Nothing -> "Invalid JSON file: "     ++ filename
                Just _  -> "Mismatched JSON value from file: " ++ filename
            Just r  -> return (r :: TopLevel)
    where
        fatal :: String -> m a
        fatal msg = liftIO $ do
            hPutStrLn stderr msg
            exitFailure

main :: IO ()
main = do
    (identifier :: String, filenames' :: [FilePath]) <- readArgs
    filenames <- case filenames' of
        [] -> do
            home <- getHomeDirectory
            pure [home </> "Dropbox" </> "General" </> "library" <.> "yaml"]
        xs -> pure xs
    forM_ filenames $ \f -> runMaybeT $ do
        library <- parseYaml f
        match <- hoistMaybe . headMay . filter ((pack identifier ==) . referencesEltId) . topLevelReferences $ library
        liftIO . BS.putStr . encodePretty (orderingReferencesElt `setConfCompare` defConfig)  $ [match]


