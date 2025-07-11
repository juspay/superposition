import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { FunctionResponse, UpdateFunctionRequest } from "../models/models_0";
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
 * The input for {@link UpdateFunctionCommand}.
 */
export interface UpdateFunctionCommandInput extends UpdateFunctionRequest {
}
/**
 * @public
 *
 * The output of {@link UpdateFunctionCommand}.
 */
export interface UpdateFunctionCommandOutput extends FunctionResponse, __MetadataBearer {
}
declare const UpdateFunctionCommand_base: {
    new (input: UpdateFunctionCommandInput): import("@smithy/smithy-client").CommandImpl<UpdateFunctionCommandInput, UpdateFunctionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: UpdateFunctionCommandInput): import("@smithy/smithy-client").CommandImpl<UpdateFunctionCommandInput, UpdateFunctionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, UpdateFunctionCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, UpdateFunctionCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // UpdateFunctionRequest
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   function_name: "STRING_VALUE", // required
 *   description: "STRING_VALUE",
 *   change_reason: "STRING_VALUE", // required
 *   function: "STRING_VALUE", // required
 *   runtime_version: "STRING_VALUE", // required
 * };
 * const command = new UpdateFunctionCommand(input);
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
 * @param UpdateFunctionCommandInput - {@link UpdateFunctionCommandInput}
 * @returns {@link UpdateFunctionCommandOutput}
 * @see {@link UpdateFunctionCommandInput} for command's `input` shape.
 * @see {@link UpdateFunctionCommandOutput} for command's `response` shape.
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
export declare class UpdateFunctionCommand extends UpdateFunctionCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: UpdateFunctionRequest;
            output: FunctionResponse;
        };
        sdk: {
            input: UpdateFunctionCommandInput;
            output: UpdateFunctionCommandOutput;
        };
    };
}
