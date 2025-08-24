module Io.Superposition.Model.CreateOrganisationInput (
    setCountryCode,
    setContactEmail,
    setContactPhone,
    setAdminEmail,
    setSector,
    setName,
    build,
    CreateOrganisationInputBuilder,
    CreateOrganisationInput,
    country_code,
    contact_email,
    contact_phone,
    admin_email,
    sector,
    name
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data CreateOrganisationInput = CreateOrganisationInput {
    country_code :: Data.Maybe.Maybe Data.Text.Text,
    contact_email :: Data.Maybe.Maybe Data.Text.Text,
    contact_phone :: Data.Maybe.Maybe Data.Text.Text,
    admin_email :: Data.Text.Text,
    sector :: Data.Maybe.Maybe Data.Text.Text,
    name :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON CreateOrganisationInput where
    toJSON a = Data.Aeson.object [
        "country_code" Data.Aeson..= country_code a,
        "contact_email" Data.Aeson..= contact_email a,
        "contact_phone" Data.Aeson..= contact_phone a,
        "admin_email" Data.Aeson..= admin_email a,
        "sector" Data.Aeson..= sector a,
        "name" Data.Aeson..= name a
        ]
    


instance Data.Aeson.FromJSON CreateOrganisationInput where
    parseJSON = Data.Aeson.withObject "CreateOrganisationInput" $ \v -> CreateOrganisationInput
        Data.Functor.<$> (v Data.Aeson..: "country_code")
        Control.Applicative.<*> (v Data.Aeson..: "contact_email")
        Control.Applicative.<*> (v Data.Aeson..: "contact_phone")
        Control.Applicative.<*> (v Data.Aeson..: "admin_email")
        Control.Applicative.<*> (v Data.Aeson..: "sector")
        Control.Applicative.<*> (v Data.Aeson..: "name")
    



data CreateOrganisationInputBuilderState = CreateOrganisationInputBuilderState {
    country_codeBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    contact_emailBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    contact_phoneBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    admin_emailBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    sectorBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: CreateOrganisationInputBuilderState
defaultBuilderState = CreateOrganisationInputBuilderState {
    country_codeBuilderState = Data.Maybe.Nothing,
    contact_emailBuilderState = Data.Maybe.Nothing,
    contact_phoneBuilderState = Data.Maybe.Nothing,
    admin_emailBuilderState = Data.Maybe.Nothing,
    sectorBuilderState = Data.Maybe.Nothing,
    nameBuilderState = Data.Maybe.Nothing
}

newtype CreateOrganisationInputBuilder a = CreateOrganisationInputBuilder {
    runCreateOrganisationInputBuilder :: CreateOrganisationInputBuilderState -> (CreateOrganisationInputBuilderState, a)
}

instance Data.Functor.Functor CreateOrganisationInputBuilder where
    fmap f (CreateOrganisationInputBuilder g) =
        CreateOrganisationInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative CreateOrganisationInputBuilder where
    pure a = CreateOrganisationInputBuilder (\s -> (s, a))
    (CreateOrganisationInputBuilder f) <*> (CreateOrganisationInputBuilder g) = CreateOrganisationInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad CreateOrganisationInputBuilder where
    (CreateOrganisationInputBuilder f) >>= g = CreateOrganisationInputBuilder (\s ->
        let (s', a) = f s
            (CreateOrganisationInputBuilder h) = g a
        in h s')

setCountryCode :: Data.Maybe.Maybe Data.Text.Text -> CreateOrganisationInputBuilder ()
setCountryCode value =
   CreateOrganisationInputBuilder (\s -> (s { country_codeBuilderState = value }, ()))

setContactEmail :: Data.Maybe.Maybe Data.Text.Text -> CreateOrganisationInputBuilder ()
setContactEmail value =
   CreateOrganisationInputBuilder (\s -> (s { contact_emailBuilderState = value }, ()))

setContactPhone :: Data.Maybe.Maybe Data.Text.Text -> CreateOrganisationInputBuilder ()
setContactPhone value =
   CreateOrganisationInputBuilder (\s -> (s { contact_phoneBuilderState = value }, ()))

setAdminEmail :: Data.Text.Text -> CreateOrganisationInputBuilder ()
setAdminEmail value =
   CreateOrganisationInputBuilder (\s -> (s { admin_emailBuilderState = Data.Maybe.Just value }, ()))

setSector :: Data.Maybe.Maybe Data.Text.Text -> CreateOrganisationInputBuilder ()
setSector value =
   CreateOrganisationInputBuilder (\s -> (s { sectorBuilderState = value }, ()))

setName :: Data.Text.Text -> CreateOrganisationInputBuilder ()
setName value =
   CreateOrganisationInputBuilder (\s -> (s { nameBuilderState = Data.Maybe.Just value }, ()))

build :: CreateOrganisationInputBuilder () -> Data.Either.Either Data.Text.Text CreateOrganisationInput
build builder = do
    let (st, _) = runCreateOrganisationInputBuilder builder defaultBuilderState
    country_code' <- Data.Either.Right (country_codeBuilderState st)
    contact_email' <- Data.Either.Right (contact_emailBuilderState st)
    contact_phone' <- Data.Either.Right (contact_phoneBuilderState st)
    admin_email' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateOrganisationInput.CreateOrganisationInput.admin_email is a required property.") Data.Either.Right (admin_emailBuilderState st)
    sector' <- Data.Either.Right (sectorBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.CreateOrganisationInput.CreateOrganisationInput.name is a required property.") Data.Either.Right (nameBuilderState st)
    Data.Either.Right (CreateOrganisationInput { 
        country_code = country_code',
        contact_email = contact_email',
        contact_phone = contact_phone',
        admin_email = admin_email',
        sector = sector',
        name = name'
    })


