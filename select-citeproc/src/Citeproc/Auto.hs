{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module Citeproc.Auto where

import           Control.Monad      (mzero, join, (<=<))
import           Data.Aeson.AutoType.Alternative
import qualified Data.Aeson as A (pairs, object)
import           Data.Aeson.Types(Pair)
import           Data.Aeson(Value(..), FromJSON(..), ToJSON(..),
                            (.:), (.:?), (.=))
import           Data.Monoid
import           Control.Applicative
import           Data.Text (Text)
import           GHC.Generics

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)

object = A.object . filter ((/= Null) . snd)
pairs = A.pairs

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


data Issued = Issued { 
    issuedDateParts :: [[Double]]
  } deriving (Show,Eq,Generic)


instance FromJSON Issued where
  parseJSON (Object v) = Issued <$> v .:   "date-parts"
  parseJSON _          = mzero


instance ToJSON Issued where
  toJSON     (Issued {..}) = object ["date-parts" .= issuedDateParts]
  toEncoding (Issued {..}) = pairs  ("date-parts" .= issuedDateParts)


data TopLevelElt = TopLevelElt { 
    topLevelEltEdition :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltISSN :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltChapterNumber :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltAnnote :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltDOI :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltPublisherPlace :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltVolume :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltCollectionNumber :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltURL :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltPage :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltISBN :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltTitleShort :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltContainerTitle :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltAuthor :: (Maybe ([AuthorElt])),
    topLevelEltId :: Text,
    topLevelEltAccessed :: (Maybe (Issued:|:[(Maybe Value)])),
    topLevelEltIssued :: (Maybe (Issued:|:[(Maybe Value)])),
    topLevelEltPMID :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltAbstract :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltTitle :: Text,
    topLevelEltType :: Text,
    topLevelEltNumber :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltGenre :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltCollectionTitle :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltPublisher :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltIssue :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltEditor :: (Maybe ([AuthorElt])),
    topLevelEltKeyword :: (Maybe (Text:|:[(Maybe Value)]))
  } deriving (Show,Eq,Generic)

orderingTopLevelElt :: Text -> Text -> Ordering
orderingTopLevelElt "id" _ = LT
orderingTopLevelElt _ "id" = GT
orderingTopLevelElt _ _ = EQ

getDOI :: TopLevelElt -> Maybe Text
getDOI = pure `alt` const empty <=< topLevelEltDOI

instance FromJSON TopLevelElt where
  parseJSON (Object v) = TopLevelElt <$> v .:?? "edition" <*> v .:?? "ISSN" <*> v .:?? "chapter-number" <*> v .:?? "annote" <*> v .:?? "DOI" <*> v .:?? "publisher-place" <*> v .:?? "volume" <*> v .:?? "collection-number" <*> v .:?? "URL" <*> v .:?? "page" <*> v .:?? "ISBN" <*> v .:?? "title-short" <*> v .:?? "container-title" <*> v .:?? "author" <*> v .:   "id" <*> v .:?? "accessed" <*> v .:?? "issued" <*> v .:?? "PMID" <*> v .:?? "abstract" <*> v .:   "title" <*> v .:   "type" <*> v .:?? "number" <*> v .:?? "genre" <*> v .:?? "collection-title" <*> v .:?? "publisher" <*> v .:?? "issue" <*> v .:?? "editor" <*> v .:?? "keyword"
  parseJSON _          = mzero


instance ToJSON TopLevelElt where
  toJSON     (TopLevelElt {..}) = object ["edition" .= topLevelEltEdition, "ISSN" .= topLevelEltISSN, "chapter-number" .= topLevelEltChapterNumber, "annote" .= topLevelEltAnnote, "DOI" .= topLevelEltDOI, "publisher-place" .= topLevelEltPublisherPlace, "volume" .= topLevelEltVolume, "collection-number" .= topLevelEltCollectionNumber, "URL" .= topLevelEltURL, "page" .= topLevelEltPage, "ISBN" .= topLevelEltISBN, "title-short" .= topLevelEltTitleShort, "container-title" .= topLevelEltContainerTitle, "author" .= topLevelEltAuthor, "id" .= topLevelEltId, "accessed" .= topLevelEltAccessed, "issued" .= topLevelEltIssued, "PMID" .= topLevelEltPMID, "abstract" .= topLevelEltAbstract, "title" .= topLevelEltTitle, "type" .= topLevelEltType, "number" .= topLevelEltNumber, "genre" .= topLevelEltGenre, "collection-title" .= topLevelEltCollectionTitle, "publisher" .= topLevelEltPublisher, "issue" .= topLevelEltIssue, "editor" .= topLevelEltEditor, "keyword" .= topLevelEltKeyword]
  toEncoding (TopLevelElt {..}) = pairs  ("edition" .= topLevelEltEdition<>"ISSN" .= topLevelEltISSN<>"chapter-number" .= topLevelEltChapterNumber<>"annote" .= topLevelEltAnnote<>"DOI" .= topLevelEltDOI<>"publisher-place" .= topLevelEltPublisherPlace<>"volume" .= topLevelEltVolume<>"collection-number" .= topLevelEltCollectionNumber<>"URL" .= topLevelEltURL<>"page" .= topLevelEltPage<>"ISBN" .= topLevelEltISBN<>"title-short" .= topLevelEltTitleShort<>"container-title" .= topLevelEltContainerTitle<>"author" .= topLevelEltAuthor<>"id" .= topLevelEltId<>"accessed" .= topLevelEltAccessed<>"issued" .= topLevelEltIssued<>"PMID" .= topLevelEltPMID<>"abstract" .= topLevelEltAbstract<>"title" .= topLevelEltTitle<>"type" .= topLevelEltType<>"number" .= topLevelEltNumber<>"genre" .= topLevelEltGenre<>"collection-title" .= topLevelEltCollectionTitle<>"publisher" .= topLevelEltPublisher<>"issue" .= topLevelEltIssue<>"editor" .= topLevelEltEditor<>"keyword" .= topLevelEltKeyword)

type TopLevel = TopLevelYaml:|:[TopLevelElt]

data TopLevelYaml = TopLevelYaml {
    topLevelYaml :: [TopLevelElt]
  } deriving (Show,Eq,Generic)

instance FromJSON TopLevelYaml where
  parseJSON (Object v) = TopLevelYaml <$> v .:   "references"
  parseJSON _          = mzero

instance ToJSON TopLevelYaml where
  toJSON     (TopLevelYaml {..}) = object ["references" .= topLevelYaml]
  toEncoding (TopLevelYaml {..}) = pairs  ("references" .= topLevelYaml)


