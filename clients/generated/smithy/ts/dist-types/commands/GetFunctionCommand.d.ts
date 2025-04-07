import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { FunctionResponse, GetFunctionInput } from "../models/models_0";
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
 * The input for {@link GetFunctionCommand}.
 */
export interface GetFunctionCommandInput extends GetFunctionInput {
}
/**
 * @public
 *
 * The output of {@link GetFunctionCommand}.
 */
export interface GetFunctionCommandOutput extends FunctionResponse, __MetadataBearer {
}
declare const GetFunctionCommand_base: {
    new (input: GetFunctionCommandInput): import("@smithy/smithy-client").CommandImpl<GetFunctionCommandInput, GetFunctionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: GetFunctionCommandInput): import("@smithy/smithy-client").CommandImpl<GetFunctionCommandInput, GetFunctionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetFunctionCommand } from "@io.juspay/superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetFunctionCommand } = require("@io.juspay/superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetFunctionInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   function_name: "STRING_VALUE", // required
 * };
 * const command = new GetFunctionCommand(input);
 * const response = await client.send(command);
 * // { // FunctionResponse
 * //   function_name: "STRING_VALUE", // required
 * //   published_code: "STRING_VALUE",
 * //   draft_code: "STRING_VALUE", // required
 * //   published_runtime_version: "STRING_VALUE",
 * //   draft_runtime_version: "STRING_VALUE", // required
 * //   published_at: new Date("TIMESTAMP"),
 * //   draft_edited_at: new Date("TIMESTAMP"), // required
 * //   published_by: "STRING_VALUE",
 * //   draft_edited_by: "STRING_VALUE", // required
 * //   last_modified_at: new Date("TIMESTAMP"), // required
 * //   last_modified_by: "STRING_VALUE", // required
 * //   change_reason: "STRING_VALUE", // required
 * //   description: "STRING_VALUE", // required
 * //   function_type: "VALIDATION" || "AUTOCOMPLETE", // required
 * // };
 *
 * ```
 *
 * @param GetFunctionCommandInput - {@link GetFunctionCommandInput}
 * @returns {@link GetFunctionCommandOutput}
 * @see {@link GetFunctionCommandInput} for command's `input` shape.
 * @see {@link GetFunctionCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link FunctionNotFound} (client fault)
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class GetFunctionCommand extends GetFunctionCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: GetFunctionInput;
            output: FunctionResponse;
        };
        sdk: {
            input: GetFunctionCommandInput;
            output: GetFunctionCommandOutput;
        };
    };
}
