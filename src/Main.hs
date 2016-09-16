{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Control.Concurrent.STM
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import Data.Text (Text)
import Network.Hive
import Text.Printf (printf)

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data Self = Self
  { nextIp :: TVar Int
  , mmeMap :: TVar MmeMap
  }

type MmeMap = Map Text [Text]

main :: IO ()
main = do
  self <- Self <$> newTVarIO 1
               <*> newTVarIO Map.empty
  hive defaultHiveConfig $ do

    -- Match a GET on the site root. Redirect to index.html.
    match GET <!> None ==> redirectTo "index.html"

    -- List all mmes.
    match GET </> "api" </> "0.1" </> "mme" <!> None
          ==> listMmes self

    -- Create a new mme.
    match POST </> "api" </> "0.1" </> "mme" <!> None
          ==> (createMme self =<< bodyByteString)

    -- Fetch the list of IP addresses for the mme.
    match GET </> "api" </> "0.1" </> "mme"
              </:> "name" </> "ip_config" <!> None
          ==> getIpConfig self

    -- Delete the mme.
    match DELETE </> "api" </> "0.1" </> "mme" </:> "name" <!> None
          ==> deleteMme self

    -- The match all clause will do static serving.
    matchAll ==> serveDirectory "."

listMmes :: Self -> Handler HandlerResponse
listMmes self = do
  mmeUrls <- map mkMmeUrl . Map.keys <$> (liftIO $ readTVarIO (mmeMap self))
  respondJSON Ok $ map (\url -> object [ "url" .= url] ) mmeUrls

createMme :: Self -> LBS.ByteString -> Handler HandlerResponse
createMme self input = do
  case decodeInput of
    Just name -> do
      inserted <- liftIO $ (atomically $ maybeInsert name)
      if inserted
        then do
          logInfo $ printf "createMme: create %s" (show name)
          respondJSON Created $ object [ "url" .= mkMmeUrl name ]
        else do
          logInfo $ printf "createMme: %s already created" (show name)
          respondText Conflict "Already created"
    Nothing   -> do
      logInfo $ printf "createMme: Cannot decode %s" (show input)
      respondText BadRequest "Cannot decode JSON"
    where
      decodeInput :: Maybe Text
      decodeInput = do
        result <- decode input
        flip parseMaybe result $ \obj ->
          obj .: "name"

      maybeInsert :: Text -> STM Bool
      maybeInsert name = do
        mmes <- readTVar (mmeMap self)
        case Map.lookup name mmes of
          Just _  -> return False
          Nothing -> do
            ip <- readTVar (nextIp self)
            modifyTVar (nextIp self) (+1)
            writeTVar (mmeMap self) (Map.insert name [mkMmeIp ip] mmes)
            return True

getIpConfig :: Self -> Handler HandlerResponse
getIpConfig self = do
  name <- capture "name"
  maybeMme <- Map.lookup name <$> (liftIO $ readTVarIO (mmeMap self))
  maybe (respondText NotFound "Not Found")
        (respondJSON Ok)
        maybeMme

deleteMme :: Self -> Handler HandlerResponse
deleteMme self = do
    name <- capture "name"
    deleted <- liftIO (atomically $ maybeDelete name)
    if deleted
        then do
            logInfo $ printf "deleteMme: delete %s" (show name)
            respondText Ok ""
        else do
            logInfo $ printf "deleteMme: not found %s" (show name)
            respondText NotFound "Not Found"
    where
      maybeDelete :: Text -> STM Bool
      maybeDelete name = do
        mmes <- readTVar (mmeMap self)
        case Map.lookup name mmes of
            Just _  -> do
                writeTVar (mmeMap self) (Map.delete name mmes)
                return True
            Nothing -> return False

mkMmeUrl :: Text -> Text
mkMmeUrl name = "/api/0.1/mme/" `T.append` name

mkMmeIp :: Int -> Text
mkMmeIp n = "192.168.1." `T.append` (T.pack $ show n)
