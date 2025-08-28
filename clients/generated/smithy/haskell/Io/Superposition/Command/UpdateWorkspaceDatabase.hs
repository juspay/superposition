module Io.Superposition.Command.UpdateWorkspaceDatabase (
    UpdateWorkspaceDatabaseError(..),
    updateWorkspaceDatabase
) where
import qualified Control.Exception
import qualified Data.Aeson
import qualified Data.Aeson.Types
import qualified Data.Bifunctor
import qualified Data.ByteString
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import qualified Data.CaseInsensitive
import qualified Data.Either
import qualified Data.Function
import qualified Data.Functor
import qualified Data.List
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.UpdateWorkspaceDatabaseInput
import qualified Io.Superposition.Model.UpdateWorkspaceDatabaseOutput
import qualified Io.Superposition.Model.WorkspaceStatus
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data UpdateWorkspaceDatabaseError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON UpdateWorkspaceDatabaseError
instance Data.Aeson.FromJSON UpdateWorkspaceDatabaseError

serUpdateWorkspaceDatabaseHEADER :: Io.Superposition.Model.UpdateWorkspaceDatabaseInput.UpdateWorkspaceDatabaseInput -> Network.HTTP.Types.Header.RequestHeaders
serUpdateWorkspaceDatabaseHEADER input =
    let 
        org_idHeader = (Io.Superposition.Model.UpdateWorkspaceDatabaseInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            org_idHeader
            ]
        
    

serUpdateWorkspaceDatabaseLABEL :: Io.Superposition.Model.UpdateWorkspaceDatabaseInput.UpdateWorkspaceDatabaseInput -> Data.ByteString.ByteString
serUpdateWorkspaceDatabaseLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "workspaces",
        (Io.Superposition.Model.UpdateWorkspaceDatabaseInput.workspace_name input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        ,
        "db",
        "update"
        ]
    

updateWorkspaceDatabase :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.UpdateWorkspaceDatabaseInput.UpdateWorkspaceDatabaseInputBuilder () -> IO (Data.Either.Either UpdateWorkspaceDatabaseError Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.UpdateWorkspaceDatabaseOutput)
updateWorkspaceDatabase client inputB = do
    let inputE = Io.Superposition.Model.UpdateWorkspaceDatabaseInput.build inputB
        baseUri = Io.Superposition.SuperpositionClient.endpointUri client
        httpManager = Io.Superposition.SuperpositionClient.httpManager client
        requestE = Network.HTTP.Client.requestFromURI @(Data.Either.Either Control.Exception.SomeException) baseUri
    
    case (inputE, requestE) of
        (Data.Either.Left err, _) -> return $ Data.Either.Left (BuilderError err)
        (_, Data.Either.Left err) -> return $ Data.Either.Left (RequestError $ Data.Text.pack $ show err)
        (Data.Either.Right input, Data.Either.Right req) -> do
            response <- Network.HTTP.Client.httpLbs (toRequest input req) httpManager
            return $ Data.Bifunctor.first (RequestError) $ deserializeResponse response
        
    
    where
        method = Network.HTTP.Types.Method.methodPost
        token = Data.Text.Encoding.encodeUtf8 $ Io.Superposition.SuperpositionClient.token client
        toRequest input req =
            req {
                Network.HTTP.Client.path = serUpdateWorkspaceDatabaseLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestHeaders = (serUpdateWorkspaceDatabaseHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.UpdateWorkspaceDatabaseOutput
deserializeResponse response = do
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    workspace_admin_emailDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "workspace_admin_email") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    auto_populate_controlDocumentE :: Bool <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "auto_populate_control") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    created_atDocumentE :: Data.Time.UTCTime <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "created_at") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    organisation_nameDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "organisation_name") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    last_modified_byDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "last_modified_by") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    created_byDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "created_by") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    config_versionDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "config_version") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    mandatory_dimensionsDocumentE :: Data.Maybe.Maybe ([] Data.Text.Text) <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "mandatory_dimensions") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    workspace_statusDocumentE :: Io.Superposition.Model.WorkspaceStatus.WorkspaceStatus <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "workspace_status") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    last_modified_atDocumentE :: Data.Time.UTCTime <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "last_modified_at") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    organisation_idDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "organisation_id") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    allow_experiment_self_approvalDocumentE :: Bool <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "allow_experiment_self_approval") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    workspace_schema_nameDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "workspace_schema_name") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    strict_modeDocumentE :: Bool <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "strict_mode") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    metricsDocumentE :: Data.Maybe.Maybe Data.Aeson.Value <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "metrics") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    workspace_nameDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "workspace_name") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.build $ do
        Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.setWorkspaceAdminEmail workspace_admin_emailDocumentE
        Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.setAutoPopulateControl auto_populate_controlDocumentE
        Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.setCreatedAt created_atDocumentE
        Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.setOrganisationName organisation_nameDocumentE
        Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.setLastModifiedBy last_modified_byDocumentE
        Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.setCreatedBy created_byDocumentE
        Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.setConfigVersion config_versionDocumentE
        Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.setMandatoryDimensions mandatory_dimensionsDocumentE
        Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.setWorkspaceStatus workspace_statusDocumentE
        Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.setLastModifiedAt last_modified_atDocumentE
        Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.setOrganisationId organisation_idDocumentE
        Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.setAllowExperimentSelfApproval allow_experiment_self_approvalDocumentE
        Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.setWorkspaceSchemaName workspace_schema_nameDocumentE
        Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.setStrictMode strict_modeDocumentE
        Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.setMetrics metricsDocumentE
        Io.Superposition.Model.UpdateWorkspaceDatabaseOutput.setWorkspaceName workspace_nameDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


