module Io.Superposition.Model.GetConfigJsonInput (
    setWorkspaceId,
    setOrgId,
    setIfModifiedSince,
    build,
    GetConfigJsonInputBuilder,
    GetConfigJsonInput,
    workspace_id,
    org_id,
    if_modified_since
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data GetConfigJsonInput = GetConfigJsonInput {
    workspace_id :: Data.Text.Text,
    org_id :: Data.Text.Text,
    if_modified_since :: Data.Maybe.Maybe Data.Time.UTCTime
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetConfigJsonInput where
    toJSON a = Data.Aeson.object [
        "workspace_id" Data.Aeson..= workspace_id a,
        "org_id" Data.Aeson..= org_id a,
        "if_modified_since" Data.Aeson..= if_modified_since a
        ]
    

instance Io.Superposition.Utility.SerializeBody GetConfigJsonInput

instance Data.Aeson.FromJSON GetConfigJsonInput where
    parseJSON = Data.Aeson.withObject "GetConfigJsonInput" $ \v -> GetConfigJsonInput
        Data.Functor.<$> (v Data.Aeson..: "workspace_id")
        Control.Applicative.<*> (v Data.Aeson..: "org_id")
        Control.Applicative.<*> (v Data.Aeson..:? "if_modified_since")
    



data GetConfigJsonInputBuilderState = GetConfigJsonInputBuilderState {
    workspace_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    org_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    if_modified_sinceBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: GetConfigJsonInputBuilderState
defaultBuilderState = GetConfigJsonInputBuilderState {
    workspace_idBuilderState = Data.Maybe.Nothing,
    org_idBuilderState = Data.Maybe.Nothing,
    if_modified_sinceBuilderState = Data.Maybe.Nothing
}

type GetConfigJsonInputBuilder = Control.Monad.State.Strict.State GetConfigJsonInputBuilderState

setWorkspaceId :: Data.Text.Text -> GetConfigJsonInputBuilder ()
setWorkspaceId value =
   Control.Monad.State.Strict.modify (\s -> (s { workspace_idBuilderState = Data.Maybe.Just value }))

setOrgId :: Data.Text.Text -> GetConfigJsonInputBuilder ()
setOrgId value =
   Control.Monad.State.Strict.modify (\s -> (s { org_idBuilderState = Data.Maybe.Just value }))

setIfModifiedSince :: Data.Maybe.Maybe Data.Time.UTCTime -> GetConfigJsonInputBuilder ()
setIfModifiedSince value =
   Control.Monad.State.Strict.modify (\s -> (s { if_modified_sinceBuilderState = value }))

build :: GetConfigJsonInputBuilder () -> Data.Either.Either Data.Text.Text GetConfigJsonInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    workspace_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetConfigJsonInput.GetConfigJsonInput.workspace_id is a required property.") Data.Either.Right (workspace_idBuilderState st)
    org_id' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.GetConfigJsonInput.GetConfigJsonInput.org_id is a required property.") Data.Either.Right (org_idBuilderState st)
    if_modified_since' <- Data.Either.Right (if_modified_sinceBuilderState st)
    Data.Either.Right (GetConfigJsonInput { 
        workspace_id = workspace_id',
        org_id = org_id',
        if_modified_since = if_modified_since'
    })


instance Io.Superposition.Utility.IntoRequestBuilder GetConfigJsonInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Io.Superposition.Utility.setPath [
            "config",
            "json"
            ]
        
        Io.Superposition.Utility.serHeader "x-workspace" (workspace_id self)
        Io.Superposition.Utility.serHeader "if-modified-since" (if_modified_since self)
        Io.Superposition.Utility.serHeader "x-org-id" (org_id self)
        

