{-# OPTIONS_GHC -with-rtsopts="-N1" #-}

module Main (main) where

import FFI.Superposition qualified as FFI
import Foreign.C.String qualified as FFIC
import Foreign.Ptr qualified as FFIC
import Test.HUnit qualified as HUnit
import Foreign.Marshal.Alloc qualified as FFIC
import Control.Concurrent
import Control.Concurrent.Async (mapConcurrently_)

main :: IO ()
main = do
  -- Individually these work.
  invalidCall
  validCall
  -- Breaks in concurrent use.
  mapConcurrently_ id [invalidCall, validCall]

validCall :: IO ()
validCall = do
  let dcStr = "{\"k1\":\"v1\"}"
  dc <- FFIC.newCString dcStr
  ctx <- FFIC.newCString "[]"
  overrides <- FFIC.newCString "{}"
  query <- FFIC.newCString "{}"
  mergeS <- FFIC.newCString "merge"
  let filt = FFIC.nullPtr
      experiments = FFIC.nullPtr
  !rc <- FFI.getResolvedConfig dc ctx overrides query mergeS filt experiments
  HUnit.assertBool "Output should be non-null." (rc /= FFIC.nullPtr)
  rcStr <- FFIC.peekCAString rc
  HUnit.assertEqual "Resolved config should be correct." rcStr dcStr
  !err <- FFI.getLastErrorMessage
  if err == FFIC.nullPtr
      then pure ()
      else do
        errStr <- FFIC.peekCAString err
        HUnit.assertFailure "Error should be null."
  putStrLn $ "Valid OK!"

invalidCall :: IO ()
invalidCall = do
  let dc = FFIC.nullPtr
      ctx = FFIC.nullPtr
      overrides = FFIC.nullPtr
      query = FFIC.nullPtr
      mergeS = FFIC.nullPtr
      filt = FFIC.nullPtr
      experiments = FFIC.nullPtr
  !rc <- FFI.getResolvedConfig dc ctx overrides query mergeS filt experiments
  HUnit.assertBool "Output should be null." (rc == FFIC.nullPtr)
  -- Sleeping to emulate stack-switch.
  threadDelay 1000
  !err <- FFI.getLastErrorMessage
  HUnit.assertBool "Error should be non-null." (err /= FFIC.nullPtr)
  errStr <- FFIC.peekCAString err
  HUnit.assertBool "Error string should be non-empty." (not (null errStr))
  putStrLn $ "Recieved Error: " ++ errStr
  FFIC.free err
  FFIC.free rc
  putStrLn $ "Invalid Call OK!"
