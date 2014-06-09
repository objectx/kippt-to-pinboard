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

import           Control.Applicative ((<$>), (<*>))
import Data.Traversable (for)
import           Control.Lens        (makeLenses, (^.), (&), (^..), (^?), to, traversed)
import           Control.Monad       (mzero)
import           Data.Aeson          ((.:), (.:?), (.!=))
import qualified Data.Aeson          as DA
import Data.Aeson.Lens (key, values, _Array, _String, _Integer, _Bool, _Primitive, Primitive (..))
import           Data.Default.Class  (Default (..))
import qualified Data.Text           as T
import qualified Data.UnixTime       as UT
import qualified Pipes as P
import qualified Pipes.Parse as PP
import qualified System.IO as IO
import  Data.ByteString.Lazy (hGetContents)
import System.FilePath (FilePath(..))
import Debug.Trace (trace, traceShow)

data KipptBookmark = KipptBookmark
    { _createdTime :: !Integer
    , _updatedTime :: !Integer
    , _isFavorited :: !Bool
    , _title       :: !T.Text
    , _url         :: !T.Text
    , _notes       :: !T.Text
    } deriving Show

makeLenses ''KipptBookmark

instance Default KipptBookmark where
    def = KipptBookmark 0 0 True "dummy" "http://example.com" "empty"

instance DA.FromJSON KipptBookmark where
    parseJSON (DA.Object v) =
        KipptBookmark <$> v .: "created"
                      <*> v .: "updated"
                      <*> v .: "is_favorite"
                      <*> v .: "title"
                      <*> v .: "url"
                      <*> v .: "notes"
    parseJSON _ = mzero

fromFile path = do
    IO.withBinaryFile path IO.ReadMode fromHandle

fromHandle :: IO.Handle -> IO [KipptBookmark]
fromHandle h = do
    input <- hGetContents h
    let json = DA.decode input :: Maybe DA.Value
    case json of
        Nothing -> return []
        Just v -> do
            return $ extract v

extract :: DA.Value -> [KipptBookmark]
extract kippt =
    kippt ^.. values . key "objects" . values . to toKippt


toKippt :: DA.Value -> KipptBookmark
toKippt v = do
    KipptBookmark (getDate v "created")
                  (getDate v "updated")
                  (getFav v)
                  (v ^. (key "title") . _String)
                  (v ^. (key "url") . _String)
                  (v ^. (key "notes") . _String)

getDate v slot =
    case v ^? key slot . _Integer of
        Nothing -> 0
        Just date -> date

getFav v =
    case v ^? key "is_favorite" . _Bool of
        Nothing -> False
        Just f -> f

extractObjects v =
    v ^.. values . key "objects"