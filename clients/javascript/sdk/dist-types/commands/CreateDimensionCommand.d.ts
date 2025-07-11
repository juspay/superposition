import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { CreateDimensionInput, DimensionExt } from "../models/models_0";
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
 * The input for {@link CreateDimensionCommand}.
 */
export interface CreateDimensionCommandInput extends CreateDimensionInput {
}
/**
 * @public
 *
 * The output of {@link CreateDimensionCommand}.
 */
export interface CreateDimensionCommandOutput extends DimensionExt, __MetadataBearer {
}
declare const CreateDimensionCommand_base: {
    new (input: CreateDimensionCommandInput): import("@smithy/smithy-client").CommandImpl<CreateDimensionCommandInput, CreateDimensionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: CreateDimensionCommandInput): import("@smithy/smithy-client").CommandImpl<CreateDimensionCommandInput, CreateDimensionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, CreateDimensionCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, CreateDimensionCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // CreateDimensionInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   dimension: "STRING_VALUE", // required
 *   position: Number("int"), // required
 *   schema: "DOCUMENT_VALUE", // required
 *   function_name: "STRING_VALUE",
 *   dependencies: [ // Dependencies
 *     "STRING_VALUE",
 *   ],
 *   description: "STRING_VALUE", // required
 *   change_reason: "STRING_VALUE", // required
 *   autocomplete_function_name: "STRING_VALUE",
 * };
 * const command = new CreateDimensionCommand(input);
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
 * //   autocomplete_function_name: "STRING_VALUE",
 * //   mandatory: true || false,
 * // };
 *
 * ```
 *
 * @param CreateDimensionCommandInput - {@link CreateDimensionCommandInput}
 * @returns {@link CreateDimensionCommandOutput}
 * @see {@link CreateDimensionCommandInput} for command's `input` shape.
 * @see {@link CreateDimensionCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class CreateDimensionCommand extends CreateDimensionCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: CreateDimensionInput;
            output: DimensionExt;
        };
        sdk: {
            input: CreateDimensionCommandInput;
            output: CreateDimensionCommandOutput;
        };
    };
}
