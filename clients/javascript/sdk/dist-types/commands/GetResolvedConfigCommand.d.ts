import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { GetResolvedConfigInput, GetResolvedConfigOutput } from "../models/models_0";
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
 * The input for {@link GetResolvedConfigCommand}.
 */
export interface GetResolvedConfigCommandInput extends GetResolvedConfigInput {
}
/**
 * @public
 *
 * The output of {@link GetResolvedConfigCommand}.
 */
export interface GetResolvedConfigCommandOutput extends GetResolvedConfigOutput, __MetadataBearer {
}
declare const GetResolvedConfigCommand_base: {
    new (input: GetResolvedConfigCommandInput): import("@smithy/smithy-client").CommandImpl<GetResolvedConfigCommandInput, GetResolvedConfigCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: GetResolvedConfigCommandInput): import("@smithy/smithy-client").CommandImpl<GetResolvedConfigCommandInput, GetResolvedConfigCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetResolvedConfigCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetResolvedConfigCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetResolvedConfigInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   prefix: "STRING_VALUE",
 *   version: "STRING_VALUE",
 *   show_reasoning: true || false,
 *   merge_strategy: "MERGE" || "REPLACE",
 *   context_id: "STRING_VALUE",
 *   context: { // ContextMap
 *     "<keys>": "DOCUMENT_VALUE",
 *   },
 * };
 * const command = new GetResolvedConfigCommand(input);
 * const response = await client.send(command);
 * // { // GetResolvedConfigOutput
 * //   config: "DOCUMENT_VALUE",
 * //   version: "STRING_VALUE",
 * //   last_modified: new Date("TIMESTAMP"),
 * //   audit_id: "STRING_VALUE",
 * // };
 *
 * ```
 *
 * @param GetResolvedConfigCommandInput - {@link GetResolvedConfigCommandInput}
 * @returns {@link GetResolvedConfigCommandOutput}
 * @see {@link GetResolvedConfigCommandInput} for command's `input` shape.
 * @see {@link GetResolvedConfigCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class GetResolvedConfigCommand extends GetResolvedConfigCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: GetResolvedConfigInput;
            output: GetResolvedConfigOutput;
        };
        sdk: {
            input: GetResolvedConfigCommandInput;
            output: GetResolvedConfigCommandOutput;
        };
    };
}
