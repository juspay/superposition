import { getHttpAuthExtensionConfiguration, resolveHttpAuthRuntimeConfig, } from "./auth/httpAuthExtensionConfiguration";
import { getHttpHandlerExtensionConfiguration, resolveHttpHandlerRuntimeConfig, } from "@smithy/protocol-http";
import { getDefaultExtensionConfiguration, resolveDefaultRuntimeConfig, } from "@smithy/smithy-client";
const asPartial = (t) => t;
export const resolveRuntimeExtensions = (runtimeConfig, extensions) => {
    const extensionConfiguration = {
        ...asPartial(getDefaultExtensionConfiguration(runtimeConfig)),
        ...asPartial(getHttpHandlerExtensionConfiguration(runtimeConfig)),
        ...asPartial(getHttpAuthExtensionConfiguration(runtimeConfig)),
    };
    extensions.forEach(extension => extension.configure(extensionConfiguration));
    return {
        ...runtimeConfig,
        ...resolveDefaultRuntimeConfig(extensionConfiguration),
        ...resolveHttpHandlerRuntimeConfig(extensionConfiguration),
        ...resolveHttpAuthRuntimeConfig(extensionConfiguration),
    };
};
