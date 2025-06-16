module Io.Superposition.Command.CreateExperimentGroup (
    CreateExperimentGroupError(..),
    createExperimentGroup
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
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Time
import qualified Io.Superposition.Model.CreateExperimentGroupInput
import qualified Io.Superposition.Model.CreateExperimentGroupOutput
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data CreateExperimentGroupError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text


serCreateExperimentGroupPAYLOAD:: Io.Superposition.Model.CreateExperimentGroupInput.CreateExperimentGroupInput -> Network.HTTP.Client.RequestBody
serCreateExperimentGroupPAYLOAD input =
    Network.HTTP.Client.RequestBodyLBS $ Data.Aeson.encode $ Data.Aeson.object [
        "change_reason" Data.Aeson..= Io.Superposition.Model.CreateExperimentGroupInput.change_reason input,
        "traffic_percentage" Data.Aeson..= Io.Superposition.Model.CreateExperimentGroupInput.traffic_percentage input,
        "name" Data.Aeson..= Io.Superposition.Model.CreateExperimentGroupInput.name input,
        "context" Data.Aeson..= Io.Superposition.Model.CreateExperimentGroupInput.context input,
        "description" Data.Aeson..= Io.Superposition.Model.CreateExperimentGroupInput.description input,
        "member_experiment_ids" Data.Aeson..= Io.Superposition.Model.CreateExperimentGroupInput.member_experiment_ids input
        ]
    

serCreateExperimentGroupHEADER :: Io.Superposition.Model.CreateExperimentGroupInput.CreateExperimentGroupInput -> Network.HTTP.Types.Header.RequestHeaders
serCreateExperimentGroupHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.CreateExperimentGroupInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.CreateExperimentGroupInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader
            ]
        
    

serCreateExperimentGroupLABEL :: Io.Superposition.Model.CreateExperimentGroupInput.CreateExperimentGroupInput -> Data.ByteString.ByteString
serCreateExperimentGroupLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "experiment-groups"
        ]
    

createExperimentGroup :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.CreateExperimentGroupInput.CreateExperimentGroupInputBuilder () -> IO (Data.Either.Either CreateExperimentGroupError Io.Superposition.Model.CreateExperimentGroupOutput.CreateExperimentGroupOutput)
createExperimentGroup client inputB = do
    let inputE = Io.Superposition.Model.CreateExperimentGroupInput.build inputB
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
                Network.HTTP.Client.path = serCreateExperimentGroupLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestBody = serCreateExperimentGroupPAYLOAD input
                , Network.HTTP.Client.requestHeaders = (serCreateExperimentGroupHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.CreateExperimentGroupOutput.CreateExperimentGroupOutput
deserializeResponse response = do
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    change_reasonDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "change_reason") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    context_hashDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "context_hash") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    traffic_percentageDocumentE :: Integer <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "traffic_percentage") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    nameDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "name") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    contextDocumentE :: Data.Map.Map Data.Text.Text Data.Aeson.Value <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "context") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    descriptionDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "description") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    member_experiment_idsDocumentE :: [] Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "member_experiment_ids") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    created_atDocumentE :: Data.Time.UTCTime <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "created_at") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    id'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "id") responseObject
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
        
    
    last_modified_atDocumentE :: Data.Time.UTCTime <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "last_modified_at") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    Io.Superposition.Model.CreateExperimentGroupOutput.build $ do
        Io.Superposition.Model.CreateExperimentGroupOutput.setChangeReason change_reasonDocumentE
        Io.Superposition.Model.CreateExperimentGroupOutput.setContextHash context_hashDocumentE
        Io.Superposition.Model.CreateExperimentGroupOutput.setTrafficPercentage traffic_percentageDocumentE
        Io.Superposition.Model.CreateExperimentGroupOutput.setName nameDocumentE
        Io.Superposition.Model.CreateExperimentGroupOutput.setContext contextDocumentE
        Io.Superposition.Model.CreateExperimentGroupOutput.setDescription descriptionDocumentE
        Io.Superposition.Model.CreateExperimentGroupOutput.setMemberExperimentIds member_experiment_idsDocumentE
        Io.Superposition.Model.CreateExperimentGroupOutput.setCreatedAt created_atDocumentE
        Io.Superposition.Model.CreateExperimentGroupOutput.setId' id'DocumentE
        Io.Superposition.Model.CreateExperimentGroupOutput.setLastModifiedBy last_modified_byDocumentE
        Io.Superposition.Model.CreateExperimentGroupOutput.setCreatedBy created_byDocumentE
        Io.Superposition.Model.CreateExperimentGroupOutput.setLastModifiedAt last_modified_atDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


