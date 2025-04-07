import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { DimensionExt, UpdateDimensionInput } from "../models/models_0";
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
 * The input for {@link UpdateDimensionCommand}.
 */
export interface UpdateDimensionCommandInput extends UpdateDimensionInput {
}
/**
 * @public
 *
 * The output of {@link UpdateDimensionCommand}.
 */
export interface UpdateDimensionCommandOutput extends DimensionExt, __MetadataBearer {
}
declare const UpdateDimensionCommand_base: {
    new (input: UpdateDimensionCommandInput): import("@smithy/smithy-client").CommandImpl<UpdateDimensionCommandInput, UpdateDimensionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: UpdateDimensionCommandInput): import("@smithy/smithy-client").CommandImpl<UpdateDimensionCommandInput, UpdateDimensionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, UpdateDimensionCommand } from "@io.juspay/superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, UpdateDimensionCommand } = require("@io.juspay/superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // UpdateDimensionInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   dimension: "STRING_VALUE", // required
 *   schema: "DOCUMENT_VALUE",
 *   position: Number("int"),
 *   function_name: "STRING_VALUE",
 *   description: "STRING_VALUE",
 *   dependencies: [ // Dependencies
 *     "STRING_VALUE",
 *   ],
 *   change_reason: "STRING_VALUE", // required
 * };
 * const command = new UpdateDimensionCommand(input);
 * const response = await client.send(command);
 * // { // DimensionExt
 * //   dimension: "STRING_VALUE", // required
 * //   position: Number("int"), // required
 * //   schema: "DOCUMENT_VALUE", // required
 * //   function_name: "STRING_VALUE",
 * //   description: "STRING_VALUE", // required
 * //   change_reason: "STRING_VALUE", // required
 * //   last_modified_at: new Date("TIMESTAMP"), // required
 * //   last_modified_by: "STRING_VALUE", // required
 * //   created_at: new Date("TIMESTAMP"), // required
 * //   created_by: "STRING_VALUE", // required
 * //   dependencies: [ // Dependencies // required
 * //     "STRING_VALUE",
 * //   ],
 * //   dependents: [ // Dependents // required
 * //     "STRING_VALUE",
 * //   ],
 * //   dependency_graph: { // Object // required
 * //     "<keys>": "DOCUMENT_VALUE",
 * //   },
 * //   mandatory: true || false,
 * // };
 *
 * ```
 *
 * @param UpdateDimensionCommandInput - {@link UpdateDimensionCommandInput}
 * @returns {@link UpdateDimensionCommandOutput}
 * @see {@link UpdateDimensionCommandInput} for command's `input` shape.
 * @see {@link UpdateDimensionCommandOutput} for command's `response` shape.
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
export declare class UpdateDimensionCommand extends UpdateDimensionCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: UpdateDimensionInput;
            output: DimensionExt;
        };
        sdk: {
            input: UpdateDimensionCommandInput;
            output: UpdateDimensionCommandOutput;
        };
    };
}
