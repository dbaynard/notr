{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module SelectCiteprocSrcCiteprocAuto where

import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Environment (getArgs)
import           Control.Monad      (forM_, mzero, join)
import           Control.Applicative
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson(decode, Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.:?), (.=), object)
import           Data.Monoid
import           Data.Text (Text)
import           GHC.Generics

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)


data AuthorElt = AuthorElt { 
    authorEltFamily :: (Maybe (Text:|:[(Maybe Value)])),
    authorEltDroppingParticle :: (Maybe (Text:|:[(Maybe Value)])),
    authorEltLiteral :: (Maybe (Text:|:[(Maybe Value)])),
    authorEltGiven :: (Maybe (Text:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON AuthorElt where
  parseJSON (Object v) = AuthorElt <$> v .:?? "family" <*> v .:?? "dropping-particle" <*> v .:?? "literal" <*> v .:?? "given"
  parseJSON _          = mzero


instance ToJSON AuthorElt where
  toJSON     (AuthorElt {..}) = object ["family" .= authorEltFamily, "dropping-particle" .= authorEltDroppingParticle, "literal" .= authorEltLiteral, "given" .= authorEltGiven]
  toEncoding (AuthorElt {..}) = pairs  ("family" .= authorEltFamily<>"dropping-particle" .= authorEltDroppingParticle<>"literal" .= authorEltLiteral<>"given" .= authorEltGiven)


data AccessedElt = AccessedElt { 
    accessedEltDay :: Text,
    accessedEltYear :: Text,
    accessedEltMonth :: Text
  } deriving (Show,Eq,Generic)


instance FromJSON AccessedElt where
  parseJSON (Object v) = AccessedElt <$> v .:   "day" <*> v .:   "year" <*> v .:   "month"
  parseJSON _          = mzero


instance ToJSON AccessedElt where
  toJSON     (AccessedElt {..}) = object ["day" .= accessedEltDay, "year" .= accessedEltYear, "month" .= accessedEltMonth]
  toEncoding (AccessedElt {..}) = pairs  ("day" .= accessedEltDay<>"year" .= accessedEltYear<>"month" .= accessedEltMonth)


data IssuedElt = IssuedElt { 
    issuedEltYear :: Text,
    issuedEltMonth :: (Maybe (Text:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON IssuedElt where
  parseJSON (Object v) = IssuedElt <$> v .:   "year" <*> v .:?? "month"
  parseJSON _          = mzero


instance ToJSON IssuedElt where
  toJSON     (IssuedElt {..}) = object ["year" .= issuedEltYear, "month" .= issuedEltMonth]
  toEncoding (IssuedElt {..}) = pairs  ("year" .= issuedEltYear<>"month" .= issuedEltMonth)


data ReferencesElt = ReferencesElt { 
    referencesEltEdition :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltISSN :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltChapterNumber :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltAnnote :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltDOI :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltPublisherPlace :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltVolume :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltCollectionNumber :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltURL :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltPage :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltISBN :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltTitleShort :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltContainerTitle :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltAuthor :: (Maybe ([AuthorElt])),
    referencesEltId :: Text,
    referencesEltAccessed :: (Maybe ([AccessedElt])),
    referencesEltIssued :: (Maybe ([IssuedElt])),
    referencesEltPMID :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltAbstract :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltTitle :: Text,
    referencesEltType :: Text,
    referencesEltNumber :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltGenre :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltCollectionTitle :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltPublisher :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltIssue :: (Maybe (Text:|:[(Maybe Value)])),
    referencesEltEditor :: (Maybe ([AuthorElt])),
    referencesEltKeyword :: (Maybe (Text:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)


instance FromJSON ReferencesElt where
  parseJSON (Object v) = ReferencesElt <$> v .:?? "edition" <*> v .:?? "ISSN" <*> v .:?? "chapter-number" <*> v .:?? "annote" <*> v .:?? "DOI" <*> v .:?? "publisher-place" <*> v .:?? "volume" <*> v .:?? "collection-number" <*> v .:?? "URL" <*> v .:?? "page" <*> v .:?? "ISBN" <*> v .:?? "title-short" <*> v .:?? "container-title" <*> v .:?? "author" <*> v .:   "id" <*> v .:?? "accessed" <*> v .:?? "issued" <*> v .:?? "PMID" <*> v .:?? "abstract" <*> v .:   "title" <*> v .:   "type" <*> v .:?? "number" <*> v .:?? "genre" <*> v .:?? "collection-title" <*> v .:?? "publisher" <*> v .:?? "issue" <*> v .:?? "editor" <*> v .:?? "keyword"
  parseJSON _          = mzero


instance ToJSON ReferencesElt where
  toJSON     (ReferencesElt {..}) = object ["edition" .= referencesEltEdition, "ISSN" .= referencesEltISSN, "chapter-number" .= referencesEltChapterNumber, "annote" .= referencesEltAnnote, "DOI" .= referencesEltDOI, "publisher-place" .= referencesEltPublisherPlace, "volume" .= referencesEltVolume, "collection-number" .= referencesEltCollectionNumber, "URL" .= referencesEltURL, "page" .= referencesEltPage, "ISBN" .= referencesEltISBN, "title-short" .= referencesEltTitleShort, "container-title" .= referencesEltContainerTitle, "author" .= referencesEltAuthor, "id" .= referencesEltId, "accessed" .= referencesEltAccessed, "issued" .= referencesEltIssued, "PMID" .= referencesEltPMID, "abstract" .= referencesEltAbstract, "title" .= referencesEltTitle, "type" .= referencesEltType, "number" .= referencesEltNumber, "genre" .= referencesEltGenre, "collection-title" .= referencesEltCollectionTitle, "publisher" .= referencesEltPublisher, "issue" .= referencesEltIssue, "editor" .= referencesEltEditor, "keyword" .= referencesEltKeyword]
  toEncoding (ReferencesElt {..}) = pairs  ("edition" .= referencesEltEdition<>"ISSN" .= referencesEltISSN<>"chapter-number" .= referencesEltChapterNumber<>"annote" .= referencesEltAnnote<>"DOI" .= referencesEltDOI<>"publisher-place" .= referencesEltPublisherPlace<>"volume" .= referencesEltVolume<>"collection-number" .= referencesEltCollectionNumber<>"URL" .= referencesEltURL<>"page" .= referencesEltPage<>"ISBN" .= referencesEltISBN<>"title-short" .= referencesEltTitleShort<>"container-title" .= referencesEltContainerTitle<>"author" .= referencesEltAuthor<>"id" .= referencesEltId<>"accessed" .= referencesEltAccessed<>"issued" .= referencesEltIssued<>"PMID" .= referencesEltPMID<>"abstract" .= referencesEltAbstract<>"title" .= referencesEltTitle<>"type" .= referencesEltType<>"number" .= referencesEltNumber<>"genre" .= referencesEltGenre<>"collection-title" .= referencesEltCollectionTitle<>"publisher" .= referencesEltPublisher<>"issue" .= referencesEltIssue<>"editor" .= referencesEltEditor<>"keyword" .= referencesEltKeyword)


data TopLevel = TopLevel { 
    topLevelReferences :: [ReferencesElt]
  } deriving (Show,Eq,Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:   "references"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["references" .= topLevelReferences]
  toEncoding (TopLevel {..}) = pairs  ("references" .= topLevelReferences)




parse :: FilePath -> IO TopLevel
parse filename = do input <- BSL.readFile filename
                    case decode input of
                      Nothing -> fatal $ case (decode input :: Maybe Value) of
                                           Nothing -> "Invalid JSON file: "     ++ filename
                                           Just v  -> "Mismatched JSON value from file: " ++ filename
                      Just r  -> return (r :: TopLevel)
  where
    fatal :: String -> IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess


