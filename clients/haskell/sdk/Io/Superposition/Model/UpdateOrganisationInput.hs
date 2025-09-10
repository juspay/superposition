module Io.Superposition.Model.UpdateOrganisationInput (
    setCountryCode,
    setContactEmail,
    setContactPhone,
    setAdminEmail,
    setSector,
    setId',
    setStatus,
    build,
    UpdateOrganisationInputBuilder,
    UpdateOrganisationInput,
    country_code,
    contact_email,
    contact_phone,
    admin_email,
    sector,
    id',
    status
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.OrgStatus
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Types.Method

data UpdateOrganisationInput = UpdateOrganisationInput {
    country_code :: Data.Maybe.Maybe Data.Text.Text,
    contact_email :: Data.Maybe.Maybe Data.Text.Text,
    contact_phone :: Data.Maybe.Maybe Data.Text.Text,
    admin_email :: Data.Maybe.Maybe Data.Text.Text,
    sector :: Data.Maybe.Maybe Data.Text.Text,
    id' :: Data.Text.Text,
    status :: Data.Maybe.Maybe Io.Superposition.Model.OrgStatus.OrgStatus
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON UpdateOrganisationInput where
    toJSON a = Data.Aeson.object [
        "country_code" Data.Aeson..= country_code a,
        "contact_email" Data.Aeson..= contact_email a,
        "contact_phone" Data.Aeson..= contact_phone a,
        "admin_email" Data.Aeson..= admin_email a,
        "sector" Data.Aeson..= sector a,
        "id" Data.Aeson..= id' a,
        "status" Data.Aeson..= status a
        ]
    

instance Io.Superposition.Utility.SerializeBody UpdateOrganisationInput

instance Data.Aeson.FromJSON UpdateOrganisationInput where
    parseJSON = Data.Aeson.withObject "UpdateOrganisationInput" $ \v -> UpdateOrganisationInput
        Data.Functor.<$> (v Data.Aeson..: "country_code")
        Control.Applicative.<*> (v Data.Aeson..: "contact_email")
        Control.Applicative.<*> (v Data.Aeson..: "contact_phone")
        Control.Applicative.<*> (v Data.Aeson..: "admin_email")
        Control.Applicative.<*> (v Data.Aeson..: "sector")
        Control.Applicative.<*> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "status")
    



data UpdateOrganisationInputBuilderState = UpdateOrganisationInputBuilderState {
    country_codeBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    contact_emailBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    contact_phoneBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    admin_emailBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    sectorBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    statusBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.OrgStatus.OrgStatus
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: UpdateOrganisationInputBuilderState
defaultBuilderState = UpdateOrganisationInputBuilderState {
    country_codeBuilderState = Data.Maybe.Nothing,
    contact_emailBuilderState = Data.Maybe.Nothing,
    contact_phoneBuilderState = Data.Maybe.Nothing,
    admin_emailBuilderState = Data.Maybe.Nothing,
    sectorBuilderState = Data.Maybe.Nothing,
    id'BuilderState = Data.Maybe.Nothing,
    statusBuilderState = Data.Maybe.Nothing
}

type UpdateOrganisationInputBuilder = Control.Monad.State.Strict.State UpdateOrganisationInputBuilderState

setCountryCode :: Data.Maybe.Maybe Data.Text.Text -> UpdateOrganisationInputBuilder ()
setCountryCode value =
   Control.Monad.State.Strict.modify (\s -> (s { country_codeBuilderState = value }))

setContactEmail :: Data.Maybe.Maybe Data.Text.Text -> UpdateOrganisationInputBuilder ()
setContactEmail value =
   Control.Monad.State.Strict.modify (\s -> (s { contact_emailBuilderState = value }))

setContactPhone :: Data.Maybe.Maybe Data.Text.Text -> UpdateOrganisationInputBuilder ()
setContactPhone value =
   Control.Monad.State.Strict.modify (\s -> (s { contact_phoneBuilderState = value }))

setAdminEmail :: Data.Maybe.Maybe Data.Text.Text -> UpdateOrganisationInputBuilder ()
setAdminEmail value =
   Control.Monad.State.Strict.modify (\s -> (s { admin_emailBuilderState = value }))

setSector :: Data.Maybe.Maybe Data.Text.Text -> UpdateOrganisationInputBuilder ()
setSector value =
   Control.Monad.State.Strict.modify (\s -> (s { sectorBuilderState = value }))

setId' :: Data.Text.Text -> UpdateOrganisationInputBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

setStatus :: Data.Maybe.Maybe Io.Superposition.Model.OrgStatus.OrgStatus -> UpdateOrganisationInputBuilder ()
setStatus value =
   Control.Monad.State.Strict.modify (\s -> (s { statusBuilderState = value }))

build :: UpdateOrganisationInputBuilder () -> Data.Either.Either Data.Text.Text UpdateOrganisationInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    country_code' <- Data.Either.Right (country_codeBuilderState st)
    contact_email' <- Data.Either.Right (contact_emailBuilderState st)
    contact_phone' <- Data.Either.Right (contact_phoneBuilderState st)
    admin_email' <- Data.Either.Right (admin_emailBuilderState st)
    sector' <- Data.Either.Right (sectorBuilderState st)
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.UpdateOrganisationInput.UpdateOrganisationInput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    status' <- Data.Either.Right (statusBuilderState st)
    Data.Either.Right (UpdateOrganisationInput { 
        country_code = country_code',
        contact_email = contact_email',
        contact_phone = contact_phone',
        admin_email = admin_email',
        sector = sector',
        id' = id'',
        status = status'
    })


instance Io.Superposition.Utility.IntoRequestBuilder UpdateOrganisationInput where
    intoRequestBuilder self = do
        Io.Superposition.Utility.setMethod Network.HTTP.Types.Method.methodPut
        Io.Superposition.Utility.setPath [
            "superposition",
            "organisations",
            Io.Superposition.Utility.serializeElement (id' self)
            ]
        
        
        Io.Superposition.Utility.serField "country_code" (country_code self)
        Io.Superposition.Utility.serField "contact_phone" (contact_phone self)
        Io.Superposition.Utility.serField "sector" (sector self)
        Io.Superposition.Utility.serField "admin_email" (admin_email self)
        Io.Superposition.Utility.serField "contact_email" (contact_email self)
        Io.Superposition.Utility.serField "status" (status self)

