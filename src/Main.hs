{-# LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell #-}

module Main (main) where

import           Control.Lens               (makeLenses, to, (&), (.~), (^.))
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Lens
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text                  as T
import           Network.HTTP.Client        (ManagerSettings (..))
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.Wreq               (responseBody)
import qualified Network.Wreq               as Wreq
import           Options.Applicative        ((<$>), (<**>), (<*>), (<>))
import qualified Options.Applicative        as OA
import qualified System.IO                  as IO
import           Text.XML                   as XML
import           Text.XML.Lens              (el, root, text)

data Config = Config
    { _user     :: !String
    , _password :: !String
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

sendToPinboard :: Config -> IO ()
sendToPinboard cfg = do
    let !mgr = managerSettings 20
    token <- getAPIToken mgr cfg
    IO.putStrLn token

managerSettings :: Integer -> ManagerSettings
managerSettings timeout = tlsManagerSettings { managerResponseTimeout = Just (fromInteger $ timeout * 1000 * 1000) }

getAPIToken :: ManagerSettings -> Config -> IO String
getAPIToken mgr cfg = do
    let opt = Wreq.defaults & Wreq.manager .~ Left (managerSettings 30)
                            & Wreq.auth .~ Wreq.basicAuth (cfg ^. user . to C8.pack) (cfg ^. password . to C8.pack)
    r <- Wreq.getWith opt "https://api.pinboard.in/v1/user/api_token"
    let xml = XML.parseLBS_ XML.def $ r ^. responseBody
    return $ xml ^. root . el "result" . text . to T.unpack

--- END OF FILE ---
