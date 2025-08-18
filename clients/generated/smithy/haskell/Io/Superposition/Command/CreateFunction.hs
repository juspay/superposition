module Io.Superposition.Command.CreateFunction (
    CreateFunctionError(..),
    createFunction
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
import qualified Io.Superposition.Model.CreateFunctionInput
import qualified Io.Superposition.Model.CreateFunctionOutput
import qualified Io.Superposition.Model.FunctionTypes
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data CreateFunctionError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON CreateFunctionError
instance Data.Aeson.FromJSON CreateFunctionError

serCreateFunctionPAYLOAD:: Io.Superposition.Model.CreateFunctionInput.CreateFunctionInput -> Network.HTTP.Client.RequestBody
serCreateFunctionPAYLOAD input =
    Network.HTTP.Client.RequestBodyLBS $ Data.Aeson.encode $ Data.Aeson.object [
        "change_reason" Data.Aeson..= Io.Superposition.Model.CreateFunctionInput.change_reason input,
        "runtime_version" Data.Aeson..= Io.Superposition.Model.CreateFunctionInput.runtime_version input,
        "function_name" Data.Aeson..= Io.Superposition.Model.CreateFunctionInput.function_name input,
        "function" Data.Aeson..= Io.Superposition.Model.CreateFunctionInput.function input,
        "description" Data.Aeson..= Io.Superposition.Model.CreateFunctionInput.description input,
        "function_type" Data.Aeson..= Io.Superposition.Model.CreateFunctionInput.function_type input
        ]
    

serCreateFunctionHEADER :: Io.Superposition.Model.CreateFunctionInput.CreateFunctionInput -> Network.HTTP.Types.Header.RequestHeaders
serCreateFunctionHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.CreateFunctionInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.CreateFunctionInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        contentType = Just [("content-type", "application/json")]
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader,
            contentType
            ]
        
    

serCreateFunctionLABEL :: Io.Superposition.Model.CreateFunctionInput.CreateFunctionInput -> Data.ByteString.ByteString
serCreateFunctionLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "function"
        ]
    

createFunction :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.CreateFunctionInput.CreateFunctionInputBuilder () -> IO (Data.Either.Either CreateFunctionError Io.Superposition.Model.CreateFunctionOutput.CreateFunctionOutput)
createFunction client inputB = do
    let inputE = Io.Superposition.Model.CreateFunctionInput.build inputB
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
                Network.HTTP.Client.path = serCreateFunctionLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestBody = serCreateFunctionPAYLOAD input
                , Network.HTTP.Client.requestHeaders = (serCreateFunctionHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.CreateFunctionOutput.CreateFunctionOutput
deserializeResponse response = do
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    published_runtime_versionDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "published_runtime_version") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    descriptionDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "description") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    function_typeDocumentE :: Io.Superposition.Model.FunctionTypes.FunctionTypes <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "function_type") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    last_modified_byDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "last_modified_by") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    last_modified_atDocumentE :: Data.Time.UTCTime <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "last_modified_at") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    published_byDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "published_by") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    change_reasonDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "change_reason") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    draft_edited_byDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "draft_edited_by") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    function_nameDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "function_name") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    draft_edited_atDocumentE :: Data.Time.UTCTime <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "draft_edited_at") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    draft_codeDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "draft_code") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    draft_runtime_versionDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "draft_runtime_version") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    published_atDocumentE :: Data.Maybe.Maybe Data.Time.UTCTime <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "published_at") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    published_codeDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "published_code") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    Io.Superposition.Model.CreateFunctionOutput.build $ do
        Io.Superposition.Model.CreateFunctionOutput.setPublishedRuntimeVersion published_runtime_versionDocumentE
        Io.Superposition.Model.CreateFunctionOutput.setDescription descriptionDocumentE
        Io.Superposition.Model.CreateFunctionOutput.setFunctionType function_typeDocumentE
        Io.Superposition.Model.CreateFunctionOutput.setLastModifiedBy last_modified_byDocumentE
        Io.Superposition.Model.CreateFunctionOutput.setLastModifiedAt last_modified_atDocumentE
        Io.Superposition.Model.CreateFunctionOutput.setPublishedBy published_byDocumentE
        Io.Superposition.Model.CreateFunctionOutput.setChangeReason change_reasonDocumentE
        Io.Superposition.Model.CreateFunctionOutput.setDraftEditedBy draft_edited_byDocumentE
        Io.Superposition.Model.CreateFunctionOutput.setFunctionName function_nameDocumentE
        Io.Superposition.Model.CreateFunctionOutput.setDraftEditedAt draft_edited_atDocumentE
        Io.Superposition.Model.CreateFunctionOutput.setDraftCode draft_codeDocumentE
        Io.Superposition.Model.CreateFunctionOutput.setDraftRuntimeVersion draft_runtime_versionDocumentE
        Io.Superposition.Model.CreateFunctionOutput.setPublishedAt published_atDocumentE
        Io.Superposition.Model.CreateFunctionOutput.setPublishedCode published_codeDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


