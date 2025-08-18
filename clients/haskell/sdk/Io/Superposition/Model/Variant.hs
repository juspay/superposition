module Io.Superposition.Model.Variant (
    setId',
    setVariantType,
    setContextId,
    setOverrideId,
    setOverrides,
    build,
    VariantBuilder,
    Variant,
    id',
    variant_type,
    context_id,
    override_id,
    overrides
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
import qualified Io.Superposition.Model.VariantType
import qualified Io.Superposition.Utility

data Variant = Variant {
    id' :: Data.Text.Text,
    variant_type :: Io.Superposition.Model.VariantType.VariantType,
    context_id :: Data.Maybe.Maybe Data.Text.Text,
    override_id :: Data.Maybe.Maybe Data.Text.Text,
    overrides :: Data.Aeson.Value
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON Variant where
    toJSON a = Data.Aeson.object [
        "id" Data.Aeson..= id' a,
        "variant_type" Data.Aeson..= variant_type a,
        "context_id" Data.Aeson..= context_id a,
        "override_id" Data.Aeson..= override_id a,
        "overrides" Data.Aeson..= overrides a
        ]
    

instance Io.Superposition.Utility.SerializeBody Variant

instance Data.Aeson.FromJSON Variant where
    parseJSON = Data.Aeson.withObject "Variant" $ \v -> Variant
        Data.Functor.<$> (v Data.Aeson..: "id")
        Control.Applicative.<*> (v Data.Aeson..: "variant_type")
        Control.Applicative.<*> (v Data.Aeson..: "context_id")
        Control.Applicative.<*> (v Data.Aeson..: "override_id")
        Control.Applicative.<*> (v Data.Aeson..: "overrides")
    



data VariantBuilderState = VariantBuilderState {
    id'BuilderState :: Data.Maybe.Maybe Data.Text.Text,
    variant_typeBuilderState :: Data.Maybe.Maybe Io.Superposition.Model.VariantType.VariantType,
    context_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    override_idBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    overridesBuilderState :: Data.Maybe.Maybe Data.Aeson.Value
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: VariantBuilderState
defaultBuilderState = VariantBuilderState {
    id'BuilderState = Data.Maybe.Nothing,
    variant_typeBuilderState = Data.Maybe.Nothing,
    context_idBuilderState = Data.Maybe.Nothing,
    override_idBuilderState = Data.Maybe.Nothing,
    overridesBuilderState = Data.Maybe.Nothing
}

type VariantBuilder = Control.Monad.State.Strict.State VariantBuilderState

setId' :: Data.Text.Text -> VariantBuilder ()
setId' value =
   Control.Monad.State.Strict.modify (\s -> (s { id'BuilderState = Data.Maybe.Just value }))

setVariantType :: Io.Superposition.Model.VariantType.VariantType -> VariantBuilder ()
setVariantType value =
   Control.Monad.State.Strict.modify (\s -> (s { variant_typeBuilderState = Data.Maybe.Just value }))

setContextId :: Data.Maybe.Maybe Data.Text.Text -> VariantBuilder ()
setContextId value =
   Control.Monad.State.Strict.modify (\s -> (s { context_idBuilderState = value }))

setOverrideId :: Data.Maybe.Maybe Data.Text.Text -> VariantBuilder ()
setOverrideId value =
   Control.Monad.State.Strict.modify (\s -> (s { override_idBuilderState = value }))

setOverrides :: Data.Aeson.Value -> VariantBuilder ()
setOverrides value =
   Control.Monad.State.Strict.modify (\s -> (s { overridesBuilderState = Data.Maybe.Just value }))

build :: VariantBuilder () -> Data.Either.Either Data.Text.Text Variant
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    id'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.Variant.Variant.id' is a required property.") Data.Either.Right (id'BuilderState st)
    variant_type' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.Variant.Variant.variant_type is a required property.") Data.Either.Right (variant_typeBuilderState st)
    context_id' <- Data.Either.Right (context_idBuilderState st)
    override_id' <- Data.Either.Right (override_idBuilderState st)
    overrides' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.Variant.Variant.overrides is a required property.") Data.Either.Right (overridesBuilderState st)
    Data.Either.Right (Variant { 
        id' = id'',
        variant_type = variant_type',
        context_id = context_id',
        override_id = override_id',
        overrides = overrides'
    })


