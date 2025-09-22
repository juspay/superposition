module Io.Superposition.Model.DimensionType (
    DimensionType(..)
) where
import qualified Control.Applicative
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.Unit
import qualified Io.Superposition.Utility

-- Union implementation for DimensionType
data DimensionType =
    Regular (Io.Superposition.Model.Unit.Unit)
    | LocalCohort (Data.Text.Text)
    | RemoteCohort (Data.Text.Text)
    deriving (
    GHC.Generics.Generic,
    GHC.Show.Show,
    Data.Eq.Eq
    )

instance Data.Aeson.ToJSON DimensionType where
    toJSON (Regular a) = Data.Aeson.object [ "REGULAR" Data.Aeson..= a ]
    toJSON (LocalCohort a) = Data.Aeson.object [ "LOCAL_COHORT" Data.Aeson..= a ]
    toJSON (RemoteCohort a) = Data.Aeson.object [ "REMOTE_COHORT" Data.Aeson..= a ]

instance Io.Superposition.Utility.SerializeBody DimensionType
instance Data.Aeson.FromJSON DimensionType where
    parseJSON = Data.Aeson.withObject "DimensionType" $ \v ->
        (Regular Data.Functor.<$> v Data.Aeson..: "REGULAR") Control.Applicative.<|>
        (LocalCohort Data.Functor.<$> v Data.Aeson..: "LOCAL_COHORT") Control.Applicative.<|>
        (RemoteCohort Data.Functor.<$> v Data.Aeson..: "REMOTE_COHORT") Control.Applicative.<|>
        fail "Could not parse DimensionType. Expected an object with one of keys: REGULAR, LOCAL_COHORT, REMOTE_COHORT."
    


