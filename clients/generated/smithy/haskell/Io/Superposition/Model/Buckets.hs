module Io.Superposition.Model.Buckets (
    setMember,
    build,
    BucketsBuilder,
    Buckets,
    member
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
import qualified Io.Superposition.Model.Bucket

data Buckets = Buckets {
    member :: Data.Maybe.Maybe Io.Superposition.Model.Bucket.Bucket
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON Buckets where
    toJSON a = Data.Aeson.object [
        "member" Data.Aeson..= member a
        ]
    


instance Data.Aeson.FromJSON Buckets where
    parseJSON = Data.Aeson.withObject "Buckets" $ \v -> Buckets
        Data.Functor.<$> (v Data.Aeson..: "member")
    



data BucketsBuilderState = BucketsBuilderState {
    memberBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.Bucket.Bucket
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: BucketsBuilderState
defaultBuilderState = BucketsBuilderState {
    memberBuilderState = Data.Maybe.Nothing
}

newtype BucketsBuilder a = BucketsBuilder {
    runBucketsBuilder :: BucketsBuilderState -> (BucketsBuilderState, a)
}

instance Data.Functor.Functor BucketsBuilder where
    fmap f (BucketsBuilder g) =
        BucketsBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative BucketsBuilder where
    pure a = BucketsBuilder (\s -> (s, a))
    (BucketsBuilder f) <*> (BucketsBuilder g) = BucketsBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad BucketsBuilder where
    (BucketsBuilder f) >>= g = BucketsBuilder (\s ->
        let (s', a) = f s
            (BucketsBuilder h) = g a
        in h s')

setMember :: Data.Maybe.Maybe Io.Superposition.Model.Bucket.Bucket -> BucketsBuilder ()
setMember value =
   BucketsBuilder (\s -> (s { memberBuilderState = value }, ()))

build :: BucketsBuilder () -> Data.Either.Either Data.Text.Text Buckets
build builder = do
    let (st, _) = runBucketsBuilder builder defaultBuilderState
    member' <- Data.Either.Right (memberBuilderState st)
    Data.Either.Right (Buckets { 
        member = member'
    })


