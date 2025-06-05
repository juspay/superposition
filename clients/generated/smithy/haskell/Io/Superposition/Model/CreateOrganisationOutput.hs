module Io.Superposition.Model.CreateOrganisationOutput (
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
    CreateOrganisationOutputBuilder,
    CreateOrganisationOutput,
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
import qualified Control.Monad
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

data CreateOrganisationOutput = CreateOrganisationOutput {
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

instance Data.Aeson.ToJSON CreateOrganisationOutput where
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
    


instance Data.Aeson.FromJSON CreateOrganisationOutput where
    parseJSON = Data.Aeson.withObject "CreateOrganisationOutput" $ \v -> CreateOrganisationOutput
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
    



data CreateOrganisationOutputBuilderState = CreateOrganisationOutputBuilderState {
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

defaultBuilderState :: CreateOrganisationOutputBuilderState
defaultBuilderState = CreateOrganisationOutputBuilderState {
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

newtype CreateOrganisationOutputBuilder a = CreateOrganisationOutputBuilder {
    runCreateOrganisationOutputBuilder :: CreateOrganisationOutputBuilderState -> (CreateOrganisationOutputBuilderState, a)
}

instance Data.Functor.Functor CreateOrganisationOutputBuilder where
    fmap f (CreateOrganisationOutputBuilder g) =
        CreateOrganisationOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative CreateOrganisationOutputBuilder where
    pure a = CreateOrganisationOutputBuilder (\s -> (s, a))
    (CreateOrganisationOutputBuilder f) <*> (CreateOrganisationOutputBuilder g) = CreateOrganisationOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad CreateOrganisationOutputBuilder where
    (CreateOrganisationOutputBuilder f) >>= g = CreateOrganisationOutputBuilder (\s ->
        let (s', a) = f s
            (CreateOrganisationOutputBuilder h) = g a
        in h s')

setId' :: Data.Text.Text -> CreateOrganisationOutputBuilder ()
setId' value =
   CreateOrganisationOutputBuilder (\s -> (s { id'BuilderState = Data.Maybe.Just value }, ()))

setName :: Data.Text.Text -> CreateOrganisationOutputBuilder ()
setName value =
   CreateOrganisationOutputBuilder (\s -> (s { nameBuilderState = Data.Maybe.Just value }, ()))

setCountryCode :: Data.Maybe.Maybe Data.Text.Text -> CreateOrganisationOutputBuilder ()
setCountryCode value =
   CreateOrganisationOutputBuilder (\s -> (s { country_codeBuilderState = value }, ()))

setContactEmail :: Data.Maybe.Maybe Data.Text.Text -> CreateOrganisationOutputBuilder ()
setContactEmail value =
   CreateOrganisationOutputBuilder (\s -> (s { contact_emailBuilderState = value }, ()))

setContactPhone :: Data.Maybe.Maybe Data.Text.Text -> CreateOrganisationOutputBuilder ()
setContactPhone value =
   CreateOrganisationOutputBuilder (\s -> (s { contact_phoneBuilderState = value }, ()))

setCreatedBy :: Data.Text.Text -> CreateOrganisationOutputBuilder ()
setCreatedBy value =
   CreateOrganisationOutputBuilder (\s -> (s { created_byBuilderState = Data.Maybe.Just value }, ()))

setAdminEmail :: Data.Text.Text -> CreateOrganisationOutputBuilder ()
setAdminEmail value =
   CreateOrganisationOutputBuilder (\s -> (s { admin_emailBuilderState = Data.Maybe.Just value }, ()))

setStatus :: Io.Superposition.Model.OrgStatus.OrgStatus -> CreateOrganisationOutputBuilder ()
setStatus value =
   CreateOrganisationOutputBuilder (\s -> (s { statusBuilderState = Data.Maybe.Just value }, ()))

setSector :: Data.Maybe.Maybe Data.Text.Text -> CreateOrganisationOutputBuilder ()
setSector value =
   CreateOrganisationOutputBuilder (\s -> (s { sectorBuilderState = value }, ()))

setCreatedAt :: Data.Time.UTCTime -> CreateOrganisationOutputBuilder ()
setCreatedAt value =
   CreateOrganisationOutputBuilder (\s -> (s { created_atBuilderState = Data.Maybe.Just value }, ()))

setUpdatedAt :: Data.Time.UTCTime -> CreateOrganisationOutputBuilder ()
setUpdatedAt value =
   CreateOrganisationOutputBuilder (\s -> (s { updated_atBuilderState = Data.Maybe.Just value }, ()))

setUpdatedBy :: Data.Text.Text -> CreateOrganisationOutputBuilder ()
setUpdatedBy value =
   CreateOrganisationOutputBuilder (\s -> (s { updated_byBuilderState = Data.Maybe.Just value }, ()))

build :: CreateOrganisationOutputBuilder () -> Data.Either.Either Data.Text.Text CreateOrganisationOutput
build builder = do
    let (st, _) = runCreateOrganisationOutputBuilder builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateOrganisationOutput.CreateOrganisationOutput.id' is a required property.") Data.Either.Right (id'BuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateOrganisationOutput.CreateOrganisationOutput.name is a required property.") Data.Either.Right (nameBuilderState st)
    country_code' <- Data.Either.Right (country_codeBuilderState st)
    contact_email' <- Data.Either.Right (contact_emailBuilderState st)
    contact_phone' <- Data.Either.Right (contact_phoneBuilderState st)
    created_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateOrganisationOutput.CreateOrganisationOutput.created_by is a required property.") Data.Either.Right (created_byBuilderState st)
    admin_email' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateOrganisationOutput.CreateOrganisationOutput.admin_email is a required property.") Data.Either.Right (admin_emailBuilderState st)
    status' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateOrganisationOutput.CreateOrganisationOutput.status is a required property.") Data.Either.Right (statusBuilderState st)
    sector' <- Data.Either.Right (sectorBuilderState st)
    created_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateOrganisationOutput.CreateOrganisationOutput.created_at is a required property.") Data.Either.Right (created_atBuilderState st)
    updated_at' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateOrganisationOutput.CreateOrganisationOutput.updated_at is a required property.") Data.Either.Right (updated_atBuilderState st)
    updated_by' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateOrganisationOutput.CreateOrganisationOutput.updated_by is a required property.") Data.Either.Right (updated_byBuilderState st)
    Data.Either.Right (CreateOrganisationOutput { 
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


