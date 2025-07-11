import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { DeleteDefaultConfigInput, DeleteDefaultConfigOutput } from "../models/models_0";
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
 * The input for {@link DeleteDefaultConfigCommand}.
 */
export interface DeleteDefaultConfigCommandInput extends DeleteDefaultConfigInput {
}
/**
 * @public
 *
 * The output of {@link DeleteDefaultConfigCommand}.
 */
export interface DeleteDefaultConfigCommandOutput extends DeleteDefaultConfigOutput, __MetadataBearer {
}
declare const DeleteDefaultConfigCommand_base: {
    new (input: DeleteDefaultConfigCommandInput): import("@smithy/smithy-client").CommandImpl<DeleteDefaultConfigCommandInput, DeleteDefaultConfigCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: DeleteDefaultConfigCommandInput): import("@smithy/smithy-client").CommandImpl<DeleteDefaultConfigCommandInput, DeleteDefaultConfigCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, DeleteDefaultConfigCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, DeleteDefaultConfigCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // DeleteDefaultConfigInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   key: "STRING_VALUE", // required
 * };
 * const command = new DeleteDefaultConfigCommand(input);
 * const response = await client.send(command);
 * // {};
 *
 * ```
 *
 * @param DeleteDefaultConfigCommandInput - {@link DeleteDefaultConfigCommandInput}
 * @returns {@link DeleteDefaultConfigCommandOutput}
 * @see {@link DeleteDefaultConfigCommandInput} for command's `input` shape.
 * @see {@link DeleteDefaultConfigCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link ResourceNotFound} (client fault)
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class DeleteDefaultConfigCommand extends DeleteDefaultConfigCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: DeleteDefaultConfigInput;
            output: {};
        };
        sdk: {
            input: DeleteDefaultConfigCommandInput;
            output: DeleteDefaultConfigCommandOutput;
        };
    };
}
