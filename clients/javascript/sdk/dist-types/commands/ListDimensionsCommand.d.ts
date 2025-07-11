import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { ListDimensionsInput, ListDimensionsOutput } from "../models/models_0";
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
 * The input for {@link ListDimensionsCommand}.
 */
export interface ListDimensionsCommandInput extends ListDimensionsInput {
}
/**
 * @public
 *
 * The output of {@link ListDimensionsCommand}.
 */
export interface ListDimensionsCommandOutput extends ListDimensionsOutput, __MetadataBearer {
}
declare const ListDimensionsCommand_base: {
    new (input: ListDimensionsCommandInput): import("@smithy/smithy-client").CommandImpl<ListDimensionsCommandInput, ListDimensionsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: ListDimensionsCommandInput): import("@smithy/smithy-client").CommandImpl<ListDimensionsCommandInput, ListDimensionsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListDimensionsCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListDimensionsCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListDimensionsInput
 *   count: Number("int"),
 *   page: Number("int"),
 *   all: true || false,
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 * };
 * const command = new ListDimensionsCommand(input);
 * const response = await client.send(command);
 * // { // ListDimensionsOutput
 * //   total_pages: Number("int"),
 * //   total_items: Number("int"),
 * //   data: [ // DimensionExtList
 * //     { // DimensionExt
 * //       dimension: "STRING_VALUE", // required
 * //       position: Number("int"), // required
 * //       schema: "DOCUMENT_VALUE", // required
 * //       function_name: "STRING_VALUE",
 * //       description: "STRING_VALUE", // required
 * //       change_reason: "STRING_VALUE", // required
 * //       last_modified_at: new Date("TIMESTAMP"), // required
 * //       last_modified_by: "STRING_VALUE", // required
 * //       created_at: new Date("TIMESTAMP"), // required
 * //       created_by: "STRING_VALUE", // required
 * //       dependencies: [ // Dependencies // required
 * //         "STRING_VALUE",
 * //       ],
 * //       dependents: [ // Dependents // required
 * //         "STRING_VALUE",
 * //       ],
 * //       dependency_graph: { // Object // required
 * //         "<keys>": "DOCUMENT_VALUE",
 * //       },
 * //       autocomplete_function_name: "STRING_VALUE",
 * //       mandatory: true || false,
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param ListDimensionsCommandInput - {@link ListDimensionsCommandInput}
 * @returns {@link ListDimensionsCommandOutput}
 * @see {@link ListDimensionsCommandInput} for command's `input` shape.
 * @see {@link ListDimensionsCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class ListDimensionsCommand extends ListDimensionsCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: ListDimensionsInput;
            output: ListDimensionsOutput;
        };
        sdk: {
            input: ListDimensionsCommandInput;
            output: ListDimensionsCommandOutput;
        };
    };
}
