module Io.Superposition.Command.ResumeExperiment (
    ResumeExperimentError(..),
    resumeExperiment
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
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ExperimentStatusType
import qualified Io.Superposition.Model.ExperimentType
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.ResumeExperimentInput
import qualified Io.Superposition.Model.ResumeExperimentOutput
import qualified Io.Superposition.Model.Variant
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data ResumeExperimentError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON ResumeExperimentError
instance Data.Aeson.FromJSON ResumeExperimentError

serResumeExperimentPAYLOAD:: Io.Superposition.Model.ResumeExperimentInput.ResumeExperimentInput -> Network.HTTP.Client.RequestBody
serResumeExperimentPAYLOAD input =
    Network.HTTP.Client.RequestBodyLBS $ Data.Aeson.encode $ Data.Aeson.object [
        "change_reason" Data.Aeson..= Io.Superposition.Model.ResumeExperimentInput.change_reason input
        ]
    

serResumeExperimentHEADER :: Io.Superposition.Model.ResumeExperimentInput.ResumeExperimentInput -> Network.HTTP.Types.Header.RequestHeaders
serResumeExperimentHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.ResumeExperimentInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.ResumeExperimentInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader
            ]
        
    

serResumeExperimentLABEL :: Io.Superposition.Model.ResumeExperimentInput.ResumeExperimentInput -> Data.ByteString.ByteString
serResumeExperimentLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "experiments",
        (Io.Superposition.Model.ResumeExperimentInput.id' input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        ,
        "resume"
        ]
    

resumeExperiment :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.ResumeExperimentInput.ResumeExperimentInputBuilder () -> IO (Data.Either.Either ResumeExperimentError Io.Superposition.Model.ResumeExperimentOutput.ResumeExperimentOutput)
resumeExperiment client inputB = do
    let inputE = Io.Superposition.Model.ResumeExperimentInput.build inputB
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
                Network.HTTP.Client.path = serResumeExperimentLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestBody = serResumeExperimentPAYLOAD input
                , Network.HTTP.Client.requestHeaders = (serResumeExperimentHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.ResumeExperimentOutput.ResumeExperimentOutput
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
        
    
    Io.Superposition.Model.ResumeExperimentOutput.build $ do
        Io.Superposition.Model.ResumeExperimentOutput.setChosenVariant chosen_variantDocumentE
        Io.Superposition.Model.ResumeExperimentOutput.setCreatedAt created_atDocumentE
        Io.Superposition.Model.ResumeExperimentOutput.setDescription descriptionDocumentE
        Io.Superposition.Model.ResumeExperimentOutput.setVariants variantsDocumentE
        Io.Superposition.Model.ResumeExperimentOutput.setLastModifiedBy last_modified_byDocumentE
        Io.Superposition.Model.ResumeExperimentOutput.setOverrideKeys override_keysDocumentE
        Io.Superposition.Model.ResumeExperimentOutput.setCreatedBy created_byDocumentE
        Io.Superposition.Model.ResumeExperimentOutput.setExperimentType experiment_typeDocumentE
        Io.Superposition.Model.ResumeExperimentOutput.setChangeReason change_reasonDocumentE
        Io.Superposition.Model.ResumeExperimentOutput.setMetricsUrl metrics_urlDocumentE
        Io.Superposition.Model.ResumeExperimentOutput.setTrafficPercentage traffic_percentageDocumentE
        Io.Superposition.Model.ResumeExperimentOutput.setName nameDocumentE
        Io.Superposition.Model.ResumeExperimentOutput.setContext contextDocumentE
        Io.Superposition.Model.ResumeExperimentOutput.setStartedAt started_atDocumentE
        Io.Superposition.Model.ResumeExperimentOutput.setExperimentGroupId experiment_group_idDocumentE
        Io.Superposition.Model.ResumeExperimentOutput.setId' id'DocumentE
        Io.Superposition.Model.ResumeExperimentOutput.setMetrics metricsDocumentE
        Io.Superposition.Model.ResumeExperimentOutput.setLastModified last_modifiedDocumentE
        Io.Superposition.Model.ResumeExperimentOutput.setStartedBy started_byDocumentE
        Io.Superposition.Model.ResumeExperimentOutput.setStatus statusDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


