import { HttpAuthExtensionConfiguration } from "./auth/httpAuthExtensionConfiguration";
import { HttpHandlerExtensionConfiguration } from "@smithy/protocol-http";
import { DefaultExtensionConfiguration } from "@smithy/types";
/**
 * @internal
 */
export interface SuperpositionExtensionConfiguration extends HttpHandlerExtensionConfiguration, DefaultExtensionConfiguration, HttpAuthExtensionConfiguration {
}
