import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { DeleteFunctionInput, DeleteFunctionOutput } from "../models/models_0";
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
 * The input for {@link DeleteFunctionCommand}.
 */
export interface DeleteFunctionCommandInput extends DeleteFunctionInput {
}
/**
 * @public
 *
 * The output of {@link DeleteFunctionCommand}.
 */
export interface DeleteFunctionCommandOutput extends DeleteFunctionOutput, __MetadataBearer {
}
declare const DeleteFunctionCommand_base: {
    new (input: DeleteFunctionCommandInput): import("@smithy/smithy-client").CommandImpl<DeleteFunctionCommandInput, DeleteFunctionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: DeleteFunctionCommandInput): import("@smithy/smithy-client").CommandImpl<DeleteFunctionCommandInput, DeleteFunctionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, DeleteFunctionCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, DeleteFunctionCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // DeleteFunctionInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   function_name: "STRING_VALUE", // required
 * };
 * const command = new DeleteFunctionCommand(input);
 * const response = await client.send(command);
 * // {};
 *
 * ```
 *
 * @param DeleteFunctionCommandInput - {@link DeleteFunctionCommandInput}
 * @returns {@link DeleteFunctionCommandOutput}
 * @see {@link DeleteFunctionCommandInput} for command's `input` shape.
 * @see {@link DeleteFunctionCommandOutput} for command's `response` shape.
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
export declare class DeleteFunctionCommand extends DeleteFunctionCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: DeleteFunctionInput;
            output: {};
        };
        sdk: {
            input: DeleteFunctionCommandInput;
            output: DeleteFunctionCommandOutput;
        };
    };
}
