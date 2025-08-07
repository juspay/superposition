module Io.Superposition.Command.CreateOrganisation (
    CreateOrganisationError(..),
    createOrganisation
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
import qualified Io.Superposition.Model.CreateOrganisationInput
import qualified Io.Superposition.Model.CreateOrganisationOutput
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.OrgStatus
import qualified Io.Superposition.SuperpositionClient
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data CreateOrganisationError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON CreateOrganisationError
instance Data.Aeson.FromJSON CreateOrganisationError

serCreateOrganisationPAYLOAD:: Io.Superposition.Model.CreateOrganisationInput.CreateOrganisationInput -> Network.HTTP.Client.RequestBody
serCreateOrganisationPAYLOAD input =
    Network.HTTP.Client.RequestBodyLBS $ Data.Aeson.encode $ Data.Aeson.object [
        "country_code" Data.Aeson..= Io.Superposition.Model.CreateOrganisationInput.country_code input,
        "contact_phone" Data.Aeson..= Io.Superposition.Model.CreateOrganisationInput.contact_phone input,
        "name" Data.Aeson..= Io.Superposition.Model.CreateOrganisationInput.name input,
        "sector" Data.Aeson..= Io.Superposition.Model.CreateOrganisationInput.sector input,
        "admin_email" Data.Aeson..= Io.Superposition.Model.CreateOrganisationInput.admin_email input,
        "contact_email" Data.Aeson..= Io.Superposition.Model.CreateOrganisationInput.contact_email input
        ]
    

serCreateOrganisationLABEL :: Io.Superposition.Model.CreateOrganisationInput.CreateOrganisationInput -> Data.ByteString.ByteString
serCreateOrganisationLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "superposition",
        "organisations"
        ]
    

createOrganisation :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.CreateOrganisationInput.CreateOrganisationInputBuilder () -> IO (Data.Either.Either CreateOrganisationError Io.Superposition.Model.CreateOrganisationOutput.CreateOrganisationOutput)
createOrganisation client inputB = do
    let inputE = Io.Superposition.Model.CreateOrganisationInput.build inputB
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
                Network.HTTP.Client.path = serCreateOrganisationLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestBody = serCreateOrganisationPAYLOAD input
                , Network.HTTP.Client.requestHeaders = [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.CreateOrganisationOutput.CreateOrganisationOutput
deserializeResponse response = do
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    country_codeDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "country_code") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    contact_phoneDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "contact_phone") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    updated_atDocumentE :: Data.Time.UTCTime <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "updated_at") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    nameDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "name") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    updated_byDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "updated_by") responseObject
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
        
    
    created_byDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "created_by") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    sectorDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "sector") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    admin_emailDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "admin_email") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    contact_emailDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "contact_email") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    statusDocumentE :: Io.Superposition.Model.OrgStatus.OrgStatus <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "status") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    Io.Superposition.Model.CreateOrganisationOutput.build $ do
        Io.Superposition.Model.CreateOrganisationOutput.setCountryCode country_codeDocumentE
        Io.Superposition.Model.CreateOrganisationOutput.setContactPhone contact_phoneDocumentE
        Io.Superposition.Model.CreateOrganisationOutput.setUpdatedAt updated_atDocumentE
        Io.Superposition.Model.CreateOrganisationOutput.setName nameDocumentE
        Io.Superposition.Model.CreateOrganisationOutput.setUpdatedBy updated_byDocumentE
        Io.Superposition.Model.CreateOrganisationOutput.setCreatedAt created_atDocumentE
        Io.Superposition.Model.CreateOrganisationOutput.setId' id'DocumentE
        Io.Superposition.Model.CreateOrganisationOutput.setCreatedBy created_byDocumentE
        Io.Superposition.Model.CreateOrganisationOutput.setSector sectorDocumentE
        Io.Superposition.Model.CreateOrganisationOutput.setAdminEmail admin_emailDocumentE
        Io.Superposition.Model.CreateOrganisationOutput.setContactEmail contact_emailDocumentE
        Io.Superposition.Model.CreateOrganisationOutput.setStatus statusDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


