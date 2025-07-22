module Io.Superposition.Model.Bucket (
    setExperimentId,
    setVariantId,
    build,
    BucketBuilder,
    Bucket,
    experiment_id,
    variant_id
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data Bucket = Bucket {
    experiment_id :: Data.Text.Text,
    variant_id :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON Bucket where
    toJSON a = Data.Aeson.object [
        "experiment_id" Data.Aeson..= experiment_id a,
        "variant_id" Data.Aeson..= variant_id a
        ]
    


instance Data.Aeson.FromJSON Bucket where
    parseJSON = Data.Aeson.withObject "Bucket" $ \v -> Bucket
        Data.Functor.<$> (v Data.Aeson..: "experiment_id")
        Control.Applicative.<*> (v Data.Aeson..: "variant_id")
    



data BucketBuilderState = BucketBuilderState {
    experiment_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    variant_idBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: BucketBuilderState
defaultBuilderState = BucketBuilderState {
    experiment_idBuilderState = Data.Maybe.Nothing,
    variant_idBuilderState = Data.Maybe.Nothing
}

newtype BucketBuilder a = BucketBuilder {
    runBucketBuilder :: BucketBuilderState -> (BucketBuilderState, a)
}

instance Data.Functor.Functor BucketBuilder where
    fmap f (BucketBuilder g) =
        BucketBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative BucketBuilder where
    pure a = BucketBuilder (\s -> (s, a))
    (BucketBuilder f) <*> (BucketBuilder g) = BucketBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad BucketBuilder where
    (BucketBuilder f) >>= g = BucketBuilder (\s ->
        let (s', a) = f s
            (BucketBuilder h) = g a
        in h s')

setExperimentId :: Data.Text.Text -> BucketBuilder ()
setExperimentId value =
   BucketBuilder (\s -> (s { experiment_idBuilderState = Data.Maybe.Just value }, ()))

setVariantId :: Data.Text.Text -> BucketBuilder ()
setVariantId value =
   BucketBuilder (\s -> (s { variant_idBuilderState = Data.Maybe.Just value }, ()))

build :: BucketBuilder () -> Data.Either.Either Data.Text.Text Bucket
build builder = do
    let (st, _) = runBucketBuilder builder defaultBuilderState
    experiment_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.Bucket.Bucket.experiment_id is a required property.") Data.Either.Right (experiment_idBuilderState st)
    variant_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.Bucket.Bucket.variant_id is a required property.") Data.Either.Right (variant_idBuilderState st)
    Data.Either.Right (Bucket { 
        experiment_id = experiment_id',
        variant_id = variant_id'
    })


