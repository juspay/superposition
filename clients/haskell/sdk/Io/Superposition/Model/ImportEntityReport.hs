module Io.Superposition.Model.ImportEntityReport (
    setCreated,
    setUpdated,
    setSkipped,
    setDeleted,
    setErrors,
    build,
    ImportEntityReportBuilder,
    ImportEntityReport,
    created,
    updated,
    skipped,
    deleted,
    errors
) where
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Int
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ImportErrorItem
import qualified Io.Superposition.Utility

data ImportEntityReport = ImportEntityReport {
    created :: Data.Int.Int32,
    updated :: Data.Int.Int32,
    skipped :: Data.Int.Int32,
    deleted :: Data.Int.Int32,
    errors :: Data.Maybe.Maybe ([] Io.Superposition.Model.ImportErrorItem.ImportErrorItem)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ImportEntityReport where
    toJSON a = Data.Aeson.object [
        "created" Data.Aeson..= created a,
        "updated" Data.Aeson..= updated a,
        "skipped" Data.Aeson..= skipped a,
        "deleted" Data.Aeson..= deleted a,
        "errors" Data.Aeson..= errors a
        ]
    

instance Io.Superposition.Utility.SerializeBody ImportEntityReport

instance Data.Aeson.FromJSON ImportEntityReport where
    parseJSON = Data.Aeson.withObject "ImportEntityReport" $ \v -> ImportEntityReport
        Data.Functor.<$> (v Data.Aeson..: "created")
        Control.Applicative.<*> (v Data.Aeson..: "updated")
        Control.Applicative.<*> (v Data.Aeson..: "skipped")
        Control.Applicative.<*> (v Data.Aeson..: "deleted")
        Control.Applicative.<*> (v Data.Aeson..:? "errors")
    



data ImportEntityReportBuilderState = ImportEntityReportBuilderState {
    createdBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    updatedBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    skippedBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    deletedBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    errorsBuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.ImportErrorItem.ImportErrorItem)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ImportEntityReportBuilderState
defaultBuilderState = ImportEntityReportBuilderState {
    createdBuilderState = Data.Maybe.Nothing,
    updatedBuilderState = Data.Maybe.Nothing,
    skippedBuilderState = Data.Maybe.Nothing,
    deletedBuilderState = Data.Maybe.Nothing,
    errorsBuilderState = Data.Maybe.Nothing
}

type ImportEntityReportBuilder = Control.Monad.State.Strict.State ImportEntityReportBuilderState

setCreated :: Data.Int.Int32 -> ImportEntityReportBuilder ()
setCreated value =
   Control.Monad.State.Strict.modify (\s -> (s { createdBuilderState = Data.Maybe.Just value }))

setUpdated :: Data.Int.Int32 -> ImportEntityReportBuilder ()
setUpdated value =
   Control.Monad.State.Strict.modify (\s -> (s { updatedBuilderState = Data.Maybe.Just value }))

setSkipped :: Data.Int.Int32 -> ImportEntityReportBuilder ()
setSkipped value =
   Control.Monad.State.Strict.modify (\s -> (s { skippedBuilderState = Data.Maybe.Just value }))

setDeleted :: Data.Int.Int32 -> ImportEntityReportBuilder ()
setDeleted value =
   Control.Monad.State.Strict.modify (\s -> (s { deletedBuilderState = Data.Maybe.Just value }))

setErrors :: Data.Maybe.Maybe ([] Io.Superposition.Model.ImportErrorItem.ImportErrorItem) -> ImportEntityReportBuilder ()
setErrors value =
   Control.Monad.State.Strict.modify (\s -> (s { errorsBuilderState = value }))

build :: ImportEntityReportBuilder () -> Data.Either.Either Data.Text.Text ImportEntityReport
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    created' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ImportEntityReport.ImportEntityReport.created is a required property.") Data.Either.Right (createdBuilderState st)
    updated' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ImportEntityReport.ImportEntityReport.updated is a required property.") Data.Either.Right (updatedBuilderState st)
    skipped' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ImportEntityReport.ImportEntityReport.skipped is a required property.") Data.Either.Right (skippedBuilderState st)
    deleted' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ImportEntityReport.ImportEntityReport.deleted is a required property.") Data.Either.Right (deletedBuilderState st)
    errors' <- Data.Either.Right (errorsBuilderState st)
    Data.Either.Right (ImportEntityReport { 
        created = created',
        updated = updated',
        skipped = skipped',
        deleted = deleted',
        errors = errors'
    })


