module Io.Superposition.Command.CreateTypeTemplates (
    CreateTypeTemplatesError(..),
    createTypeTemplates
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
import qualified Io.Superposition.Model.CreateTypeTemplatesInput
import qualified Io.Superposition.Model.CreateTypeTemplatesOutput
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data CreateTypeTemplatesError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON CreateTypeTemplatesError
instance Data.Aeson.FromJSON CreateTypeTemplatesError

serCreateTypeTemplatesPAYLOAD:: Io.Superposition.Model.CreateTypeTemplatesInput.CreateTypeTemplatesInput -> Network.HTTP.Client.RequestBody
serCreateTypeTemplatesPAYLOAD input =
    Network.HTTP.Client.RequestBodyLBS $ Data.Aeson.encode $ Data.Aeson.object [
        "type_name" Data.Aeson..= Io.Superposition.Model.CreateTypeTemplatesInput.type_name input,
        "change_reason" Data.Aeson..= Io.Superposition.Model.CreateTypeTemplatesInput.change_reason input,
        "type_schema" Data.Aeson..= Io.Superposition.Model.CreateTypeTemplatesInput.type_schema input,
        "description" Data.Aeson..= Io.Superposition.Model.CreateTypeTemplatesInput.description input
        ]
    

serCreateTypeTemplatesHEADER :: Io.Superposition.Model.CreateTypeTemplatesInput.CreateTypeTemplatesInput -> Network.HTTP.Types.Header.RequestHeaders
serCreateTypeTemplatesHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.CreateTypeTemplatesInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.CreateTypeTemplatesInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        contentType = Just [("content-type", "application/json")]
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader,
            contentType
            ]
        
    

serCreateTypeTemplatesLABEL :: Io.Superposition.Model.CreateTypeTemplatesInput.CreateTypeTemplatesInput -> Data.ByteString.ByteString
serCreateTypeTemplatesLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "types"
        ]
    

createTypeTemplates :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.CreateTypeTemplatesInput.CreateTypeTemplatesInputBuilder () -> IO (Data.Either.Either CreateTypeTemplatesError Io.Superposition.Model.CreateTypeTemplatesOutput.CreateTypeTemplatesOutput)
createTypeTemplates client inputB = do
    let inputE = Io.Superposition.Model.CreateTypeTemplatesInput.build inputB
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
                Network.HTTP.Client.path = serCreateTypeTemplatesLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestBody = serCreateTypeTemplatesPAYLOAD input
                , Network.HTTP.Client.requestHeaders = (serCreateTypeTemplatesHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.CreateTypeTemplatesOutput.CreateTypeTemplatesOutput
deserializeResponse response = do
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    type_nameDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "type_name") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    change_reasonDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "change_reason") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    type_schemaDocumentE :: Data.Aeson.Value <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "type_schema") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    descriptionDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "description") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    created_atDocumentE :: Data.Time.UTCTime <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "created_at") responseObject
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
        
    
    Io.Superposition.Model.CreateTypeTemplatesOutput.build $ do
        Io.Superposition.Model.CreateTypeTemplatesOutput.setTypeName type_nameDocumentE
        Io.Superposition.Model.CreateTypeTemplatesOutput.setChangeReason change_reasonDocumentE
        Io.Superposition.Model.CreateTypeTemplatesOutput.setTypeSchema type_schemaDocumentE
        Io.Superposition.Model.CreateTypeTemplatesOutput.setDescription descriptionDocumentE
        Io.Superposition.Model.CreateTypeTemplatesOutput.setCreatedAt created_atDocumentE
        Io.Superposition.Model.CreateTypeTemplatesOutput.setLastModifiedBy last_modified_byDocumentE
        Io.Superposition.Model.CreateTypeTemplatesOutput.setCreatedBy created_byDocumentE
        Io.Superposition.Model.CreateTypeTemplatesOutput.setLastModifiedAt last_modified_atDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


