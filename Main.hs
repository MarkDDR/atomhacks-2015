-- Quote Database a la bash.org
-- For Atomhacks 2015

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Web.Scotty
import Database.PostgreSQL.Simple
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Maybe
import Data.Word
import GHC.Generics
import Data.String

data ConfigFile =
    ConfigFile  { name          :: String
                , owner         :: String
                , email         :: String
                , databaseHost  :: String -- I would separate the database stuff
                , databasePort  :: Int    -- into its own JSON object but the parser
                , databaseUser  :: String -- didn't like that for some reason.
                , databasePass  :: String -- Something to do later
                , databaseName  :: String 
                } deriving (Show, Generic)
instance FromJSON ConfigFile
instance ToJSON ConfigFile

configFile :: FilePath
configFile = "config.json"

readConfig :: IO B.ByteString
readConfig = B.readFile configFile

parseConfig :: B.ByteString -> ConfigFile
parseConfig jsonFile = if isNothing decodedFile
                        then error "Invalid Configuration File"
                        else fromJust decodedFile
                        where decodedFile = decode jsonFile

-- For postgresql port
portIntToWord16 :: Int -> Word16
portIntToWord16 = fromInteger . toInteger

extractConnectInfo :: ConfigFile -> ConnectInfo
extractConnectInfo conf = ConnectInfo dbHost dbPort dbUser dbPass dbName
                            where
                                dbHost = databaseHost conf
                                dbPort = portIntToWord16 . databasePort $ conf
                                dbUser = databaseUser conf
                                dbPass = databasePass conf
                                dbName = databaseName conf
                                

-- create tables, if they do not already exist
dbMakeTables conn = do execute_ conn . fromString . unlines  $ ["CREATE TABLE IF NOT EXISTS quotes(",
                                                "dateUploaded TIMESTAMP NOT NULL,",
                                                "upVote INT NOT NULL,",
                                                "downVote INT NOT NULL,",
                                                "quote TEXT NOT NULL,",
                                                "reported BOOLEAN NOT NULL",
                                                ");"]
                       execute_ conn . fromString . unlines $ ["CREATE TABLE IF NOT EXISTS users(",
                                                "username VARCHAR(16) UNIQUE NOT NULL,",
                                                "password CHAR(128) NOT NULL,",
                                                "isAdmin BOOLEAN NOT NULL",
                                                ");"]
                       execute_ conn . fromString . unlines $ ["CREATE TABLE IF NOT EXISTS announcments(",
                                                "dateUploaded TIMESTAMP NOT NULL,",
                                                "username VARCHAR(16) NOT NULL,",
                                                "message TEXT NOT NULL",
                                                ");"]

main = do
    rawConfig <- readConfig
    let config = parseConfig rawConfig
    let connectInfo = extractConnectInfo config
    db <- connect connectInfo
    _ <- dbMakeTables db
    scotty 3000 $ do
        get "/"         $ do text "Homepage"
        get "/about"    $ do text . TL.pack . show $ config
        get "/:id"      $ do id <- param "id" :: ActionM TL.Text
                             text . TL.pack . show $ id
