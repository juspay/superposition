module Io.Superposition.Command.DiscardExperiment (
    DiscardExperimentError(..),
    discardExperiment
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
import qualified Io.Superposition.Model.DiscardExperimentInput
import qualified Io.Superposition.Model.DiscardExperimentOutput
import qualified Io.Superposition.Model.ExperimentStatusType
import qualified Io.Superposition.Model.ExperimentType
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.Variant
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data DiscardExperimentError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text


serDiscardExperimentPAYLOAD:: Io.Superposition.Model.DiscardExperimentInput.DiscardExperimentInput -> Network.HTTP.Client.RequestBody
serDiscardExperimentPAYLOAD input =
    Network.HTTP.Client.RequestBodyLBS $ Data.Aeson.encode $ Data.Aeson.object [
        "change_reason" Data.Aeson..= Io.Superposition.Model.DiscardExperimentInput.change_reason input
        ]
    

serDiscardExperimentHEADER :: Io.Superposition.Model.DiscardExperimentInput.DiscardExperimentInput -> Network.HTTP.Types.Header.RequestHeaders
serDiscardExperimentHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.DiscardExperimentInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.DiscardExperimentInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader
            ]
        
    

serDiscardExperimentLABEL :: Io.Superposition.Model.DiscardExperimentInput.DiscardExperimentInput -> Data.ByteString.ByteString
serDiscardExperimentLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "experiments",
        (Io.Superposition.Model.DiscardExperimentInput.id' input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        ,
        "discard"
        ]
    

discardExperiment :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.DiscardExperimentInput.DiscardExperimentInputBuilder () -> IO (Data.Either.Either DiscardExperimentError Io.Superposition.Model.DiscardExperimentOutput.DiscardExperimentOutput)
discardExperiment client inputB = do
    let inputE = Io.Superposition.Model.DiscardExperimentInput.build inputB
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
        method = Network.HTTP.Types.Method.methodPatch
        token = Data.Text.Encoding.encodeUtf8 $ Io.Superposition.SuperpositionClient.token client
        toRequest input req =
            req {
                Network.HTTP.Client.path = serDiscardExperimentLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestBody = serDiscardExperimentPAYLOAD input
                , Network.HTTP.Client.requestHeaders = (serDiscardExperimentHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.DiscardExperimentOutput.DiscardExperimentOutput
deserializeResponse response = do
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    chosen_variantDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "chosen_variant") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    created_atDocumentE :: Data.Time.UTCTime <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "created_at") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    descriptionDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "description") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    variantsDocumentE :: [] Io.Superposition.Model.Variant.Variant <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "variants") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    last_modified_byDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "last_modified_by") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    override_keysDocumentE :: [] Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "override_keys") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    created_byDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "created_by") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    experiment_typeDocumentE :: Io.Superposition.Model.ExperimentType.ExperimentType <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "experiment_type") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    change_reasonDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "change_reason") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    metrics_urlDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "metrics_url") responseObject
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
        
    
    started_atDocumentE :: Data.Maybe.Maybe Data.Time.UTCTime <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "started_at") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    experiment_group_idDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "experiment_group_id") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    id'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "id") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    metricsDocumentE :: Data.Maybe.Maybe Data.Aeson.Value <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "metrics") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    last_modifiedDocumentE :: Data.Time.UTCTime <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "last_modified") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    started_byDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "started_by") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    statusDocumentE :: Io.Superposition.Model.ExperimentStatusType.ExperimentStatusType <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "status") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    Io.Superposition.Model.DiscardExperimentOutput.build $ do
        Io.Superposition.Model.DiscardExperimentOutput.setChosenVariant chosen_variantDocumentE
        Io.Superposition.Model.DiscardExperimentOutput.setCreatedAt created_atDocumentE
        Io.Superposition.Model.DiscardExperimentOutput.setDescription descriptionDocumentE
        Io.Superposition.Model.DiscardExperimentOutput.setVariants variantsDocumentE
        Io.Superposition.Model.DiscardExperimentOutput.setLastModifiedBy last_modified_byDocumentE
        Io.Superposition.Model.DiscardExperimentOutput.setOverrideKeys override_keysDocumentE
        Io.Superposition.Model.DiscardExperimentOutput.setCreatedBy created_byDocumentE
        Io.Superposition.Model.DiscardExperimentOutput.setExperimentType experiment_typeDocumentE
        Io.Superposition.Model.DiscardExperimentOutput.setChangeReason change_reasonDocumentE
        Io.Superposition.Model.DiscardExperimentOutput.setMetricsUrl metrics_urlDocumentE
        Io.Superposition.Model.DiscardExperimentOutput.setTrafficPercentage traffic_percentageDocumentE
        Io.Superposition.Model.DiscardExperimentOutput.setName nameDocumentE
        Io.Superposition.Model.DiscardExperimentOutput.setContext contextDocumentE
        Io.Superposition.Model.DiscardExperimentOutput.setStartedAt started_atDocumentE
        Io.Superposition.Model.DiscardExperimentOutput.setExperimentGroupId experiment_group_idDocumentE
        Io.Superposition.Model.DiscardExperimentOutput.setId' id'DocumentE
        Io.Superposition.Model.DiscardExperimentOutput.setMetrics metricsDocumentE
        Io.Superposition.Model.DiscardExperimentOutput.setLastModified last_modifiedDocumentE
        Io.Superposition.Model.DiscardExperimentOutput.setStartedBy started_byDocumentE
        Io.Superposition.Model.DiscardExperimentOutput.setStatus statusDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


