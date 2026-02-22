// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ListGroupedDefaultConfigsInput,
  ListGroupedDefaultConfigsOutput,
} from "../models/models_0";
import {
  de_ListGroupedDefaultConfigsCommand,
  se_ListGroupedDefaultConfigsCommand,
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
 * The input for {@link ListGroupedDefaultConfigsCommand}.
 */
export interface ListGroupedDefaultConfigsCommandInput extends ListGroupedDefaultConfigsInput {}
/**
 * @public
 *
 * The output of {@link ListGroupedDefaultConfigsCommand}.
 */
export interface ListGroupedDefaultConfigsCommandOutput extends ListGroupedDefaultConfigsOutput, __MetadataBearer {}

/**
 * Retrieves a paginated list of all default config entries in the workspace, including their values, schemas, and metadata.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListGroupedDefaultConfigsCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListGroupedDefaultConfigsCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListGroupedDefaultConfigsInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   count: Number("int"),
 *   page: Number("int"),
 *   all: true || false,
 *   name: [ // StringList
 *     "STRING_VALUE",
 *   ],
 *   prefix: "STRING_VALUE",
 *   sort_by: "desc" || "asc",
 *   sort_on: "key" || "created_at" || "last_modified_at",
 * };
 * const command = new ListGroupedDefaultConfigsCommand(input);
 * const response = await client.send(command);
 * // { // ListGroupedDefaultConfigsOutput
 * //   total_pages: Number("int"), // required
 * //   total_items: Number("int"), // required
 * //   data: [ // ListGroupedDefaultConfigOut // required
 * //     { // GroupedDefaultConfig Union: only one key present
 * //       Group: "STRING_VALUE",
 * //       Config: { // DefaultConfigResponse
 * //         key: "STRING_VALUE", // required
 * //         value: "DOCUMENT_VALUE", // required
 * //         schema: { // Object // required
 * //           "<keys>": "DOCUMENT_VALUE",
 * //         },
 * //         description: "STRING_VALUE", // required
 * //         change_reason: "STRING_VALUE", // required
 * //         value_validation_function_name: "STRING_VALUE",
 * //         value_compute_function_name: "STRING_VALUE",
 * //         created_at: new Date("TIMESTAMP"), // required
 * //         created_by: "STRING_VALUE", // required
 * //         last_modified_at: new Date("TIMESTAMP"), // required
 * //         last_modified_by: "STRING_VALUE", // required
 * //       },
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param ListGroupedDefaultConfigsCommandInput - {@link ListGroupedDefaultConfigsCommandInput}
 * @returns {@link ListGroupedDefaultConfigsCommandOutput}
 * @see {@link ListGroupedDefaultConfigsCommandInput} for command's `input` shape.
 * @see {@link ListGroupedDefaultConfigsCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class ListGroupedDefaultConfigsCommand extends $Command.classBuilder<ListGroupedDefaultConfigsCommandInput, ListGroupedDefaultConfigsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "ListGroupedDefaultConfigs", {

  })
  .n("SuperpositionClient", "ListGroupedDefaultConfigsCommand")
  .f(void 0, void 0)
  .ser(se_ListGroupedDefaultConfigsCommand)
  .de(de_ListGroupedDefaultConfigsCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: ListGroupedDefaultConfigsInput;
      output: ListGroupedDefaultConfigsOutput;
  };
  sdk: {
      input: ListGroupedDefaultConfigsCommandInput;
      output: ListGroupedDefaultConfigsCommandOutput;
  };
};
}
