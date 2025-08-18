module Io.Superposition.Model.ContextActionOut (
    ContextActionOut(..)
) where
import qualified Control.Applicative
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ContextResponse
import qualified Io.Superposition.Utility

-- Union implementation for ContextActionOut
data ContextActionOut =
    Put (Io.Superposition.Model.ContextResponse.ContextResponse)
    | Replace (Io.Superposition.Model.ContextResponse.ContextResponse)
    | Delete (Data.Text.Text)
    | Move (Io.Superposition.Model.ContextResponse.ContextResponse)
    deriving (
    GHC.Generics.Generic,
    GHC.Show.Show,
    Data.Eq.Eq
    )

instance Data.Aeson.ToJSON ContextActionOut where
    toJSON (Put a) = Data.Aeson.object [ "PUT" Data.Aeson..= a ]
    toJSON (Replace a) = Data.Aeson.object [ "REPLACE" Data.Aeson..= a ]
    toJSON (Delete a) = Data.Aeson.object [ "DELETE" Data.Aeson..= a ]
    toJSON (Move a) = Data.Aeson.object [ "MOVE" Data.Aeson..= a ]

instance Io.Superposition.Utility.SerializeBody ContextActionOut
instance Data.Aeson.FromJSON ContextActionOut where
    parseJSON = Data.Aeson.withObject "ContextActionOut" $ \v ->
        (Put Data.Functor.<$> v Data.Aeson..: "PUT") Control.Applicative.<|>
        (Replace Data.Functor.<$> v Data.Aeson..: "REPLACE") Control.Applicative.<|>
        (Delete Data.Functor.<$> v Data.Aeson..: "DELETE") Control.Applicative.<|>
        (Move Data.Functor.<$> v Data.Aeson..: "MOVE") Control.Applicative.<|>
        fail "Could not parse ContextActionOut. Expected an object with one of keys: PUT, REPLACE, DELETE, MOVE."
    


