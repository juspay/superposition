module FFI.Superposition where

import Foreign.C.String (CString)

foreign import capi "superposition_core.h core_get_resolved_config"
  getResolvedConfig ::
    CString -> -- ^ default_config_json
    CString -> -- ^ context_json
    CString -> -- ^ overrides_json
    CString -> -- ^ query_data_json
    CString -> -- ^ merge_strategy_json
    CString -> -- ^ filter_prefixes_json (optional)
    CString -> -- ^ experimentation_json (optional)
    IO CString -- ^ resolved config json

foreign import capi "superposition_core.h core_last_error_message" getLastErrorMessage :: IO CString
