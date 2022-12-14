{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Types (Dimension(..)) where

import qualified Data.Aeson as A
import GHC.Generics

data Dimension = Dimension 
    { id :: String
    , name :: Maybe String
    , priority :: Maybe String 
    -- , sdk_version :: Text
    -- , merchant :: Text
    -- , cug :: A.Value -- A JSON value represented as a Haskell value.
    -- , os :: Text
    }
    deriving (Generic , A.FromJSON , A.ToJSON)