{-# LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell #-}

module Main (main) where

import           Control.Applicative        (some)
import           Control.Lens               (makeLenses, to, zoom, (&), (.~),
                                             (^.))
import           Control.Monad              (forM_)
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Lens            (key, _String)
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Data.Default.Class         (def)
import qualified Data.Text                  as T
import qualified Data.Text.IO as T
import           Network.HTTP.Client        (ManagerSettings (..))
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.Wreq               (responseBody)
import qualified Network.Wreq               as Wreq
import           Options.Applicative        ((<$>), (<**>), (<*>), (<>))
import qualified Options.Applicative        as OA
import           Pipes                      ((>->))
import qualified Pipes                      as P
import qualified Pipes.Aeson                as PA
import qualified Pipes.ByteString           as PB
import qualified Pipes.Parse                as PP
import qualified Data.UnixTime as UT
import qualified System.IO                  as IO

import           Data.Kippt                 as DK

data Config = Config
    { _user     :: !String
    , _password :: !String
    , _files    :: ![String]
    } deriving Show

makeLenses ''Config

-- | The program entry.
main :: IO ()
main = OA.execParser progConfig >>= sendToPinboard

progConfig :: OA.ParserInfo Config
progConfig = OA.info (config <**> OA.helper) (OA.fullDesc <> OA.progDesc "Import kippt bookmarks into pinboard")
  where
    config :: OA.Parser Config
    config = Config <$> OA.strOption (  OA.long "user"
                                     <> OA.metavar "USER"
                                     <> OA.help "Pinboard user name"
                                     <> OA.value "example"
                                     )
                    <*> OA.strOption (  OA.long "password"
                                     <> OA.metavar "PASSWORD"
                                     <> OA.help "Pinboard password"
                                     <> OA.value "We who about to die salute you!")
                    <*> some (OA.argument OA.str (OA.metavar "FILE..."))

sendToPinboard :: Config -> IO ()
sendToPinboard cfg = do
    let !mgr = managerSettings 20
    token <- getAPIToken mgr cfg
    IO.putStrLn token
    bookmarks <- DK.fromFile $ head $ cfg ^. files
    mapM_ printBookmark $ bookmarks

printBookmark :: DK.KipptBookmark -> IO ()
printBookmark x = do
    C8.putStr $ x ^. DK.createdTime . to (UT.formatUnixTimeGMT "%FT%TZ" . UT.fromEpochTime . fromIntegral)
    IO.putStr " "
    C8.putStr $ x ^. DK.updatedTime . to (UT.formatUnixTimeGMT "%FT%TZ" . UT.fromEpochTime . fromIntegral)
    IO.putStr " "
    T.putStr $ x ^. DK.url
    IO.putStr " "
    T.putStrLn $ x ^. DK.title

managerSettings :: Integer -> ManagerSettings
managerSettings timeout = tlsManagerSettings { managerResponseTimeout = Just (fromInteger $ timeout * 1000 * 1000) }

getAPIToken :: ManagerSettings -> Config -> IO String
getAPIToken mgr cfg = do
    let opt = Wreq.defaults & Wreq.manager .~ Left (managerSettings 30)
                            & Wreq.auth .~ Wreq.basicAuth (cfg ^. user . to C8.pack) (cfg ^. password . to C8.pack)
    r <- Wreq.getWith opt "https://api.pinboard.in/v1/user/api_token?format=json"
    return $ r ^. responseBody . key "result" . _String . to T.unpack

readBookmarks :: Config -> IO ()
readBookmarks cfg = undefined

--- END OF FILE ---
