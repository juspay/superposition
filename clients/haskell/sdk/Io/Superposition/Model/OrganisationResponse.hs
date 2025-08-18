module Io.Superposition.Model.OrganisationResponse (
    setId',
    setName,
    setCountryCode,
    setContactEmail,
    setContactPhone,
    setCreatedBy,
    setAdminEmail,
    setStatus,
    setSector,
    setCreatedAt,
    setUpdatedAt,
    setUpdatedBy,
    build,
    OrganisationResponseBuilder,
    OrganisationResponse,
    id',
    name,
    country_code,
    contact_email,
    contact_phone,
    created_by,
    admin_email,
    status,
    sector,
    created_at,
    updated_at,
    updated_by
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
import qualified Io.Superposition.Model.OrgStatus
import qualified Io.Superposition.Utility

data OrganisationResponse = OrganisationResponse {
    id' :: Data.Text.Text,
    name :: Data.Text.Text,
    country_code :: Data.Maybe.Maybe Data.Text.Text,
    contact_email :: Data.Maybe.Maybe Data.Text.Text,
    contact_phone :: Data.Maybe.Maybe Data.Text.Text,
    created_by :: Data.Text.Text,
    admin_email :: Data.Text.Text,
    status :: Io.Superposition.Model.OrgStatus.OrgStatus,
    sector :: Data.Maybe.Maybe Data.Text.Text,
    created_at :: Data.Time.UTCTime,
    updated_at :: Data.Time.UTCTime,
    updated_by :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON OrganisationResponse where
    toJSON a = Data.Aeson.object [
        "id" Data.Aeson..= id' a,
        "name" Data.Aeson..= name a,
        "country_code" Data.Aeson..= country_code a,
        "contact_email" Data.Aeson..= contact_email a,
        "contact_phone" Data.Aeson..= contact_phone a,
        "created_by" Data.Aeson..= created_by a,
        "admin_email" Data.Aeson..= admin_email a,
        "status" Data.Aeson..= status a,
        "sector" Data.Aeson..= sector a,
        "created_at" Data.Aeson..= created_at a,
        "updated_at" Data.Aeson..= updated_at a,
        "updated_by" Data.Aeson..= updated_by a
        ]
    

instance Io.Superposition.Utility.SerializeBody OrganisationResponse

instance Data.Aeson.FromJSON OrganisationResponse where
    parseJSON = Data.Aeson.withObject "OrganisationResponse" $ \v -> OrganisationResponse
        Data.Functor.<$> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "name")
        Control.Applicative.<*> (v Data.Aeson..: "country_code")
        Control.Applicative.<*> (v Data.Aeson..: "contact_email")
        Control.Applicative.<*> (v Data.Aeson..: "contact_phone")
        Control.Applicative.<*> (v Data.Aeson..: "created_by")
        Control.Applicative.<*> (v Data.Aeson..: "admin_email")
        Control.Applicative.<*> (v Data.Aeson..: "status")
        Control.Applicative.<*> (v Data.Aeson..: "sector")
        Control.Applicative.<*> (v Data.Aeson..: "created_at")
        Control.Applicative.<*> (v Data.Aeson..: "updated_at")
        Control.Applicative.<*> (v Data.Aeson..: "updated_by")
    



data OrganisationResponseBuilderState = OrganisationResponseBuilderState {
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    country_codeBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    contact_emailBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    contact_phoneBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_byBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    admin_emailBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    statusBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.OrgStatus.OrgStatus,
    sectorBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    created_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    updated_atBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    updated_byBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: OrganisationResponseBuilderState
defaultBuilderState = OrganisationResponseBuilderState {
    id'BuilderState = Data.Maybe.Nothing,
    nameBuilderState = Data.Maybe.Nothing,
    country_codeBuilderState = Data.Maybe.Nothing,
    contact_emailBuilderState = Data.Maybe.Nothing,
    contact_phoneBuilderState = Data.Maybe.Nothing,
    created_byBuilderState = Data.Maybe.Nothing,
    admin_emailBuilderState = Data.Maybe.Nothing,
    statusBuilderState = Data.Maybe.Nothing,
    sectorBuilderState = Data.Maybe.Nothing,
    created_atBuilderState = Data.Maybe.Nothing,
    updated_atBuilderState = Data.Maybe.Nothing,
    updated_byBuilderState = Data.Maybe.Nothing
}

type OrganisationResponseBuilder = Control.Monad.State.Strict.State OrganisationResponseBuilderState

setId' :: Data.Text.Text -> OrganisationResponseBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

setName :: Data.Text.Text -> OrganisationResponseBuilder ()
setName value =
   Control.Monad.State.Strict.modify (\s -> (s { nameBuilderState = Data.Maybe.Just value }))

setCountryCode :: Data.Maybe.Maybe Data.Text.Text -> OrganisationResponseBuilder ()
setCountryCode value =
   Control.Monad.State.Strict.modify (\s -> (s { country_codeBuilderState = value }))

setContactEmail :: Data.Maybe.Maybe Data.Text.Text -> OrganisationResponseBuilder ()
setContactEmail value =
   Control.Monad.State.Strict.modify (\s -> (s { contact_emailBuilderState = value }))

setContactPhone :: Data.Maybe.Maybe Data.Text.Text -> OrganisationResponseBuilder ()
setContactPhone value =
   Control.Monad.State.Strict.modify (\s -> (s { contact_phoneBuilderState = value }))

setCreatedBy :: Data.Text.Text -> OrganisationResponseBuilder ()
setCreatedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { created_byBuilderState = Data.Maybe.Just value }))

setAdminEmail :: Data.Text.Text -> OrganisationResponseBuilder ()
setAdminEmail value =
   Control.Monad.State.Strict.modify (\s -> (s { admin_emailBuilderState = Data.Maybe.Just value }))

setStatus :: Io.Superposition.Model.OrgStatus.OrgStatus -> OrganisationResponseBuilder ()
setStatus value =
   Control.Monad.State.Strict.modify (\s -> (s { statusBuilderState = Data.Maybe.Just value }))

setSector :: Data.Maybe.Maybe Data.Text.Text -> OrganisationResponseBuilder ()
setSector value =
   Control.Monad.State.Strict.modify (\s -> (s { sectorBuilderState = value }))

setCreatedAt :: Data.Time.UTCTime -> OrganisationResponseBuilder ()
setCreatedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { created_atBuilderState = Data.Maybe.Just value }))

setUpdatedAt :: Data.Time.UTCTime -> OrganisationResponseBuilder ()
setUpdatedAt value =
   Control.Monad.State.Strict.modify (\s -> (s { updated_atBuilderState = Data.Maybe.Just value }))

setUpdatedBy :: Data.Text.Text -> OrganisationResponseBuilder ()
setUpdatedBy value =
   Control.Monad.State.Strict.modify (\s -> (s { updated_byBuilderState = Data.Maybe.Just value }))

build :: OrganisationResponseBuilder () -> Data.Either.Either Data.Text.Text OrganisationResponse
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.OrganisationResponse.OrganisationResponse.id' is a required property.") Data.Either.Right (id'BuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.OrganisationResponse.OrganisationResponse.name is a required property.") Data.Either.Right (nameBuilderState st)
    country_code' <- Data.Either.Right (country_codeBuilderState st)
    contact_email' <- Data.Either.Right (contact_emailBuilderState st)
    contact_phone' <- Data.Either.Right (contact_phoneBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.OrganisationResponse.OrganisationResponse.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    admin_email' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.OrganisationResponse.OrganisationResponse.admin_email is a required property.") Data.Either.Right (admin_emailBuilderState st)
    status' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.OrganisationResponse.OrganisationResponse.status is a required property.") Data.Either.Right (statusBuilderState st)
    sector' <- Data.Either.Right (sectorBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.OrganisationResponse.OrganisationResponse.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    updated_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.OrganisationResponse.OrganisationResponse.updated_at is a required property.") Data.Either.Right (updated_atBuilderState st)
    updated_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.OrganisationResponse.OrganisationResponse.updated_by is a required property.") Data.Either.Right (updated_byBuilderState st)
    Data.Either.Right (OrganisationResponse { 
        id' = id'',
        name = name',
        country_code = country_code',
        contact_email = contact_email',
        contact_phone = contact_phone',
        created_by = created_by',
        admin_email = admin_email',
        status = status',
        sector = sector',
        created_at = created_at',
        updated_at = updated_at',
        updated_by = updated_by'
    })


