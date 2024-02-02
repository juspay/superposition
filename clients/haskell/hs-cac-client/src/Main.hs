{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           Client             (cacEval, createCacClient, getCacClient,
                                     getCacConfig, getCacLastModified, cacStartPolling)
import           Control.Concurrent
import           Prelude

main :: IO ()
main = do
    createCacClient "dev" True 10 "http://localhost:8080" >>= \case
        Left err -> putStrLn err
        Right _  -> pure ()
    threadId <- forkOS (cacStartPolling "dev")
    print threadId
    getCacClient "dev" >>= \case
        Left err     -> putStrLn err
        Right client -> do
            config       <- getCacConfig client
            lastModified <- getCacLastModified client
            overrides    <- cacEval client "{\"os\": \"android\", \"client\": \"2mg\"}"
            print config
            print lastModified
            print overrides
            threadDelay 1000000000
    pure ()
