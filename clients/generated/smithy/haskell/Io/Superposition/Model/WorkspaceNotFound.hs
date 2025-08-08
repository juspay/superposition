module Io.Superposition.Model.WorkspaceNotFound (
    build,
    WorkspaceNotFoundBuilder,
    WorkspaceNotFound
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data WorkspaceNotFound = WorkspaceNotFound {
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON WorkspaceNotFound where
    toJSON a = Data.Aeson.object [
        ]
    


instance Data.Aeson.FromJSON WorkspaceNotFound where
    parseJSON = Data.Aeson.withObject "WorkspaceNotFound" $ \_ -> pure $ WorkspaceNotFound



data WorkspaceNotFoundBuilderState = WorkspaceNotFoundBuilderState {
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: WorkspaceNotFoundBuilderState
defaultBuilderState = WorkspaceNotFoundBuilderState {
}

newtype WorkspaceNotFoundBuilder a = WorkspaceNotFoundBuilder {
    runWorkspaceNotFoundBuilder :: WorkspaceNotFoundBuilderState -> (WorkspaceNotFoundBuilderState, a)
}

instance Data.Functor.Functor WorkspaceNotFoundBuilder where
    fmap f (WorkspaceNotFoundBuilder g) =
        WorkspaceNotFoundBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative WorkspaceNotFoundBuilder where
    pure a = WorkspaceNotFoundBuilder (\s -> (s, a))
    (WorkspaceNotFoundBuilder f) <*> (WorkspaceNotFoundBuilder g) = WorkspaceNotFoundBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad WorkspaceNotFoundBuilder where
    (WorkspaceNotFoundBuilder f) >>= g = WorkspaceNotFoundBuilder (\s ->
        let (s', a) = f s
            (WorkspaceNotFoundBuilder h) = g a
        in h s')


build :: WorkspaceNotFoundBuilder () -> Data.Either.Either Data.Text.Text WorkspaceNotFound
build builder = do
    let (st, _) = runWorkspaceNotFoundBuilder builder defaultBuilderState
    Data.Either.Right (WorkspaceNotFound { 
    })


