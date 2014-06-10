{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Data.Kippt
    ( KipptBookmark
    , createdTime
    , updatedTime
    , isFavorited
    , title
    , url
    , notes
    , fromHandle
    , fromFile
    )
    where

import           Control.Applicative  ((<$>), (<*>))
import           Control.Lens         (makeLenses, to, (^..))
import           Control.Monad        (mzero)
import           Data.Aeson           ((.:))
import qualified Data.Aeson           as DA
import           Data.Aeson.Lens      (key, values)
import           Data.Aeson.Types     (parseMaybe)
import           Data.ByteString.Lazy (hGetContents)
import           Data.Default.Class   (Default (..))
import           Data.Maybe           (catMaybes)
import qualified Data.Text            as T
import           System.FilePath      ()
import qualified System.IO            as IO

data KipptBookmark = KipptBookmark
    { _createdTime :: !Integer
    , _updatedTime :: !Integer
    , _isFavorited :: !Bool
    , _title       :: !T.Text
    , _url         :: !T.Text
    , _notes       :: !(Maybe T.Text)
    } deriving Show

makeLenses ''KipptBookmark

instance Default KipptBookmark where
    def = KipptBookmark 0 0 True "dummy" "http://example.com" Nothing

instance DA.FromJSON KipptBookmark where
    parseJSON (DA.Object v) =
        KipptBookmark <$> v .: "created"
                      <*> v .: "updated"
                      <*> v .: "is_favorite"
                      <*> v .: "title"
                      <*> v .: "url"
                      <*> v .: "notes"
    parseJSON _ = mzero

fromFile :: FilePath -> IO [KipptBookmark]
fromFile path = do
    IO.withBinaryFile path IO.ReadMode fromHandle

fromHandle :: IO.Handle -> IO [KipptBookmark]
fromHandle h = do
    input <- hGetContents h
    case DA.decode input :: Maybe DA.Value of
        Nothing -> return []
        Just v  -> return $ catMaybes $ v ^.. values . key "objects" . values . to toKippt

toKippt :: DA.Value -> Maybe KipptBookmark
toKippt v = parseMaybe DA.parseJSON v :: Maybe KipptBookmark
