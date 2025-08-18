module Io.Superposition.Model.ContextAction (
    ContextAction(..)
) where
import qualified Control.Applicative
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ContextMove
import qualified Io.Superposition.Model.ContextPut
import qualified Io.Superposition.Model.UpdateContextOverrideRequest
import qualified Io.Superposition.Utility

-- Union implementation for ContextAction
data ContextAction =
    Put (Io.Superposition.Model.ContextPut.ContextPut)
    | Replace (Io.Superposition.Model.UpdateContextOverrideRequest.UpdateContextOverrideRequest)
    | Delete (Data.Text.Text)
    | Move (Io.Superposition.Model.ContextMove.ContextMove)
    deriving (
    GHC.Generics.Generic,
    GHC.Show.Show,
    Data.Eq.Eq
    )

instance Data.Aeson.ToJSON ContextAction where
    toJSON (Put a) = Data.Aeson.object [ "PUT" Data.Aeson..= a ]
    toJSON (Replace a) = Data.Aeson.object [ "REPLACE" Data.Aeson..= a ]
    toJSON (Delete a) = Data.Aeson.object [ "DELETE" Data.Aeson..= a ]
    toJSON (Move a) = Data.Aeson.object [ "MOVE" Data.Aeson..= a ]

instance Io.Superposition.Utility.SerializeBody ContextAction
instance Data.Aeson.FromJSON ContextAction where
    parseJSON = Data.Aeson.withObject "ContextAction" $ \v ->
        (Put Data.Functor.<$> v Data.Aeson..: "PUT") Control.Applicative.<|>
        (Replace Data.Functor.<$> v Data.Aeson..: "REPLACE") Control.Applicative.<|>
        (Delete Data.Functor.<$> v Data.Aeson..: "DELETE") Control.Applicative.<|>
        (Move Data.Functor.<$> v Data.Aeson..: "MOVE") Control.Applicative.<|>
        fail "Could not parse ContextAction. Expected an object with one of keys: PUT, REPLACE, DELETE, MOVE."
    


