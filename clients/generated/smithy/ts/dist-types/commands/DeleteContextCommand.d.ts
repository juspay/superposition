import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { DeleteContextInput, DeleteContextOutput } from "../models/models_0";
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
 * The input for {@link DeleteContextCommand}.
 */
export interface DeleteContextCommandInput extends DeleteContextInput {
}
/**
 * @public
 *
 * The output of {@link DeleteContextCommand}.
 */
export interface DeleteContextCommandOutput extends DeleteContextOutput, __MetadataBearer {
}
declare const DeleteContextCommand_base: {
    new (input: DeleteContextCommandInput): import("@smithy/smithy-client").CommandImpl<DeleteContextCommandInput, DeleteContextCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: DeleteContextCommandInput): import("@smithy/smithy-client").CommandImpl<DeleteContextCommandInput, DeleteContextCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, DeleteContextCommand } from "@io.juspay/superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, DeleteContextCommand } = require("@io.juspay/superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // DeleteContextInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   id: "STRING_VALUE", // required
 *   config_tags: "STRING_VALUE",
 * };
 * const command = new DeleteContextCommand(input);
 * const response = await client.send(command);
 * // {};
 *
 * ```
 *
 * @param DeleteContextCommandInput - {@link DeleteContextCommandInput}
 * @returns {@link DeleteContextCommandOutput}
 * @see {@link DeleteContextCommandInput} for command's `input` shape.
 * @see {@link DeleteContextCommandOutput} for command's `response` shape.
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
export declare class DeleteContextCommand extends DeleteContextCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: DeleteContextInput;
            output: {};
        };
        sdk: {
            input: DeleteContextCommandInput;
            output: DeleteContextCommandOutput;
        };
    };
}
