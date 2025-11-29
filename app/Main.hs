{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Server.Routes
import Server.SSE
import Network.Wai.Handler.Warp
import Control.Monad
import Control.Concurrent
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types 
import qualified Data.ByteString.Char8 as BS
import System.Environment
import Control.Exception
import System.IO
import Text.Printf

runMigration :: Connection -> FilePath -> IO ()
runMigration conn fp = do
  sql <- readFile fp
  void $ execute_ conn (Query $ BS.pack sql)

getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault key defaultValue = do
  maybeValue <- lookupEnv key
  return $ maybe defaultValue id maybeValue

connectWithRetry :: BS.ByteString -> Int -> Int -> IO Connection
connectWithRetry connString maxRetries delaySeconds = do
  let tryConnect attempt = do
        result <- try (connectPostgreSQL connString)
        case result of
          Right conn -> return conn
          Left err -> do
            if attempt >= maxRetries
              then do
                hPutStrLn stderr $ "Error connecting to database after " ++ show maxRetries ++ " attempts:"
                hPutStrLn stderr $ show (err :: SomeException)
                throwIO err
              else do
                hPutStrLn stderr $ printf "Attempt %d/%d: Database not ready, waiting %d seconds..." (attempt + 1) maxRetries delaySeconds
                threadDelay (delaySeconds * 1000000)
                tryConnect (attempt + 1)
  tryConnect 0

main :: IO () 
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    
    putStrLn "Starting server on port 8080..."

    dbHost <- getEnvWithDefault "DB_HOST" "localhost"
    dbPort <- getEnvWithDefault "DB_PORT" "5432"
    dbName <- getEnvWithDefault "DB_NAME" "haskservant_db"
    dbUser <- getEnvWithDefault "DB_USER" "haskservant"
    dbPassword <- getEnvWithDefault "DB_PASSWORD" "haskservant123"

    putStrLn $ "Connecting to database: " ++ dbName ++ " at " ++ dbHost ++ ":" ++ dbPort

    let connString = BS.pack $ 
          "host=" ++ dbHost ++ " " ++
          "port=" ++ dbPort ++ " " ++
          "dbname=" ++ dbName ++ " " ++
          "user=" ++ dbUser ++ " " ++
          "password=" ++ dbPassword

    conn <- connectWithRetry connString 30 2

    putStrLn "Connection established successfully!"
    putStrLn "Running migrations..."

    runMigration conn "migration.sql"

    putStrLn "Migrations completed!"
    
    broadcast <- createBroadcastChannel
    putStrLn "Broadcast channel created"
    putStrLn "Server running on port 8080"

    run 8080 (app conn broadcast)
