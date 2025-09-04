module Io.Superposition.Model.ContextIdentifier (
    ContextIdentifier(..)
) where
import qualified Control.Applicative
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Union implementation for ContextIdentifier
data ContextIdentifier =
    Id' (Data.Text.Text)
    | Context (Data.Map.Map Data.Text.Text Data.Aeson.Value)
    deriving (
    GHC.Generics.Generic,
    GHC.Show.Show,
    Data.Eq.Eq
    )

instance Data.Aeson.ToJSON ContextIdentifier where
    toJSON (Id' a) = Data.Aeson.object [ "id" Data.Aeson..= a ]
    toJSON (Context a) = Data.Aeson.object [ "context" Data.Aeson..= a ]

instance Io.Superposition.Utility.SerializeBody ContextIdentifier
instance Data.Aeson.FromJSON ContextIdentifier where
    parseJSON = Data.Aeson.withObject "ContextIdentifier" $ \v ->
        (Id' Data.Functor.<$> v Data.Aeson..: "id") Control.Applicative.<|>
        (Context Data.Functor.<$> v Data.Aeson..: "context") Control.Applicative.<|>
        fail "Could not parse ContextIdentifier. Expected an object with one of keys: id', context."
    


