{-# OPTIONS_GHC -with-rtsopts="-N1" #-}

module Main (main) where

import FFI.Superposition qualified as FFI
import Test.HUnit qualified as HUnit

main :: IO HUnit.Counts
main = do
  HUnit.runTestTT $
    HUnit.TestList
      [ HUnit.TestLabel "Valid Call" $ HUnit.TestCase validCall,
        HUnit.TestLabel "In-Valid Call" $ HUnit.TestCase invalidCall
      ]

validCall :: IO ()
validCall = do
  let orig = "{\"k1\":\"v1\"}"
      params =
        FFI.defaultResolveParams
          { FFI.defaultConfig = Just orig,
            FFI.context = Just "[]",
            FFI.overrides = Just "{}",
            FFI.dimensionInfo = Just "{}",
            FFI.query = Just "{}"
          }
  result <- FFI.getResolvedConfig params
  case result of
    Right cfg -> HUnit.assertEqual ("Resolved config is in-correct: " ++ cfg) cfg orig
    Left e -> HUnit.assertFailure $ "Recieved error in valid call: " ++ e

invalidCall :: IO ()
invalidCall = do
  result <- FFI.getResolvedConfig FFI.defaultResolveParams
  case result of
    Right _ -> HUnit.assertFailure $ "Expected error, recieved: " ++ show result
    Left e -> HUnit.assertBool "Error should not be empty." (not $ null e)
