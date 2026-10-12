module Io.Superposition.Model.GroupedDefaultConfig (
    GroupedDefaultConfig(..)
) where
import qualified Control.Applicative
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.DefaultConfigResponse
import qualified Io.Superposition.Utility

-- Union implementation for GroupedDefaultConfig
data GroupedDefaultConfig =
    Group (Data.Text.Text)
    | Config (Io.Superposition.Model.DefaultConfigResponse.DefaultConfigResponse)
    deriving (
    GHC.Generics.Generic,
    GHC.Show.Show,
    Data.Eq.Eq
    )

instance Data.Aeson.ToJSON GroupedDefaultConfig where
    toJSON (Group a) = Data.Aeson.object [ "Group" Data.Aeson..= a ]
    toJSON (Config a) = Data.Aeson.object [ "Config" Data.Aeson..= a ]

instance Io.Superposition.Utility.SerializeBody GroupedDefaultConfig
instance Data.Aeson.FromJSON GroupedDefaultConfig where
    parseJSON = Data.Aeson.withObject "GroupedDefaultConfig" $ \v ->
        (Group Data.Functor.<$> v Data.Aeson..: "Group") Control.Applicative.<|>
        (Config Data.Functor.<$> v Data.Aeson..: "Config") Control.Applicative.<|>
        fail "Could not parse GroupedDefaultConfig. Expected an object with one of keys: Group, Config."
    


