import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { GetConfigInput, GetConfigOutput } from "../models/models_0";
import { Command as $Command } from "@smithy/smithy-client";
import { MetadataBearer as __MetadataBearer } from "@smithy/types";
/**
 * @public
 */
export type { __MetadataBearer };
export { $Command };
/**
 * @public
 *
 * The input for {@link GetConfigCommand}.
 */
export interface GetConfigCommandInput extends GetConfigInput {
}
/**
 * @public
 *
 * The output of {@link GetConfigCommand}.
 */
export interface GetConfigCommandOutput extends GetConfigOutput, __MetadataBearer {
}
declare const GetConfigCommand_base: {
    new (input: GetConfigCommandInput): import("@smithy/smithy-client").CommandImpl<GetConfigCommandInput, GetConfigCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: GetConfigCommandInput): import("@smithy/smithy-client").CommandImpl<GetConfigCommandInput, GetConfigCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetConfigCommand } from "@io.juspay/superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetConfigCommand } = require("@io.juspay/superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetConfigInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   prefix: "STRING_VALUE",
 *   version: "STRING_VALUE",
 *   context: { // ContextMap
 *     "<keys>": "DOCUMENT_VALUE",
 *   },
 * };
 * const command = new GetConfigCommand(input);
 * const response = await client.send(command);
 * // { // GetConfigOutput
 * //   contexts: [ // ContextList
 * //     { // ContextPartial
 * //       id: "STRING_VALUE",
 * //       condition: { // Condition
 * //         "<keys>": "DOCUMENT_VALUE",
 * //       },
 * //       priority: Number("int"),
 * //       weight: Number("int"),
 * //       override_with_keys: [ // OverrideWithKeys
 * //         "STRING_VALUE",
 * //       ],
 * //     },
 * //   ],
 * //   overrides: { // OverridesMap
 * //     "<keys>": { // Overrides
 * //       "<keys>": "DOCUMENT_VALUE",
 * //     },
 * //   },
 * //   default_configs: { // Object
 * //     "<keys>": "DOCUMENT_VALUE",
 * //   },
 * //   version: "STRING_VALUE",
 * //   last_modified: new Date("TIMESTAMP"),
 * //   audit_id: "STRING_VALUE",
 * // };
 *
 * ```
 *
 * @param GetConfigCommandInput - {@link GetConfigCommandInput}
 * @returns {@link GetConfigCommandOutput}
 * @see {@link GetConfigCommandInput} for command's `input` shape.
 * @see {@link GetConfigCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class GetConfigCommand extends GetConfigCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: GetConfigInput;
            output: GetConfigOutput;
        };
        sdk: {
            input: GetConfigCommandInput;
            output: GetConfigCommandOutput;
        };
    };
}
