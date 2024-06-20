{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           Client             (createExpClient, expStartPolling,
                                     getApplicableVariants, getExpClient,
                                     getFilteredSatisfiedExperiments,
                                     getRunningExperiments,
                                     getSatisfiedExperiments)
import           Control.Concurrent
import           Prelude

main :: IO ()
main = do
    createExpClient "dev" 10 "http://localhost:8080" >>= \case
        Left err -> putStrLn err
        Right _  -> pure ()
    threadId <- forkIO (expStartPolling "dev")
    print threadId
    getExpClient "dev" >>= \case
        Left err     -> putStrLn err
        Right client -> loop client
    pure ()
    where
        loop client = do
            runningExperiments   <- getRunningExperiments client
            satisfiedExperiments <- getSatisfiedExperiments client "{\"os\": \"android\", \"client\": \"1mg\"}" Nothing
            filteredExperiments <- getFilteredSatisfiedExperiments client (Just "{\"os\": \"android\"}") (Just "hyperpay")
            variants             <- getApplicableVariants client "{\"os\": \"android\", \"client\": \"1mg\"}" 9
            print "Running experiments"
            print runningExperiments
            print "experiments that satisfy context"
            print satisfiedExperiments
            print "experiments after filtering"
            print filteredExperiments
            print "variant ID applied"
            print variants
            -- threadDelay 10000000
            loop client
