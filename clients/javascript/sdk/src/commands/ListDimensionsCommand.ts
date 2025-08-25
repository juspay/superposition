// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ListDimensionsInput,
  ListDimensionsOutput,
} from "../models/models_0";
import {
  de_ListDimensionsCommand,
  se_ListDimensionsCommand,
} from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
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
export interface ListDimensionsCommandInput extends ListDimensionsInput {}
/**
 * @public
 *
 * The output of {@link ListDimensionsCommand}.
 */
export interface ListDimensionsCommandOutput extends ListDimensionsOutput, __MetadataBearer {}

/**
 * Retrieves a paginated list of all dimensions in the workspace. Dimensions are returned with their details and metadata.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListDimensionsCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListDimensionsCommand } = require("superposition-sdk"); // CommonJS import
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
 * @public
 */
export class ListDimensionsCommand extends $Command.classBuilder<ListDimensionsCommandInput, ListDimensionsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "ListDimensions", {

  })
  .n("SuperpositionClient", "ListDimensionsCommand")
  .f(void 0, void 0)
  .ser(se_ListDimensionsCommand)
  .de(de_ListDimensionsCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
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
