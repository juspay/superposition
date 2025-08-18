module Io.Superposition.Model.OptionalBucket (
    OptionalBucket(..)
) where
import qualified Control.Applicative
import qualified Data.Aeson
import qualified Data.Eq
import qualified Data.Functor
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.Bucket
import qualified Io.Superposition.Model.Unit

-- Union implementation for OptionalBucket
data OptionalBucket =
    Bucket (Io.Superposition.Model.Bucket.Bucket)
    | Null (Io.Superposition.Model.Unit.Unit)
    deriving (
    GHC.Generics.Generic,
    GHC.Show.Show,
    Data.Eq.Eq
    )

instance Data.Aeson.ToJSON OptionalBucket where
    toJSON (Bucket a) = Data.Aeson.object [ "bucket" Data.Aeson..= a ]
    toJSON (Null a) = Data.Aeson.object [ "null" Data.Aeson..= a ]

instance Data.Aeson.FromJSON OptionalBucket where
    parseJSON = Data.Aeson.withObject "OptionalBucket" $ \v ->
        (Bucket Data.Functor.<$> v Data.Aeson..: "bucket") Control.Applicative.<|>
        (Null Data.Functor.<$> v Data.Aeson..: "null") Control.Applicative.<|>
        fail "Could not parse OptionalBucket. Expected an object with one of keys: bucket, null."
    


