// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ListVariablesInput,
  ListVariablesOutput,
} from "../models/models_0";
import {
  de_ListVariablesCommand,
  se_ListVariablesCommand,
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
 * The input for {@link ListVariablesCommand}.
 */
export interface ListVariablesCommandInput extends ListVariablesInput {}
/**
 * @public
 *
 * The output of {@link ListVariablesCommand}.
 */
export interface ListVariablesCommandOutput extends ListVariablesOutput, __MetadataBearer {}

/**
 * Retrieves a paginated list of all variables in the workspace with optional filtering and sorting.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListVariablesCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListVariablesCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListVariablesInput
 *   count: Number("int"),
 *   page: Number("int"),
 *   all: true || false,
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   name: [ // StringList
 *     "STRING_VALUE",
 *   ],
 *   created_by: [
 *     "STRING_VALUE",
 *   ],
 *   last_modified_by: [
 *     "STRING_VALUE",
 *   ],
 *   sort_on: "name" || "created_at" || "last_modified_at",
 *   sort_by: "desc" || "asc",
 * };
 * const command = new ListVariablesCommand(input);
 * const response = await client.send(command);
 * // { // ListVariablesOutput
 * //   total_pages: Number("int"), // required
 * //   total_items: Number("int"), // required
 * //   data: [ // VariableList // required
 * //     { // VariableResponse
 * //       name: "STRING_VALUE", // required
 * //       value: "STRING_VALUE", // required
 * //       description: "STRING_VALUE", // required
 * //       change_reason: "STRING_VALUE", // required
 * //       created_by: "STRING_VALUE", // required
 * //       created_at: new Date("TIMESTAMP"), // required
 * //       last_modified_by: "STRING_VALUE", // required
 * //       last_modified_at: new Date("TIMESTAMP"), // required
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param ListVariablesCommandInput - {@link ListVariablesCommandInput}
 * @returns {@link ListVariablesCommandOutput}
 * @see {@link ListVariablesCommandInput} for command's `input` shape.
 * @see {@link ListVariablesCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class ListVariablesCommand extends $Command.classBuilder<ListVariablesCommandInput, ListVariablesCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "ListVariables", {

  })
  .n("SuperpositionClient", "ListVariablesCommand")
  .f(void 0, void 0)
  .ser(se_ListVariablesCommand)
  .de(de_ListVariablesCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: ListVariablesInput;
      output: ListVariablesOutput;
  };
  sdk: {
      input: ListVariablesCommandInput;
      output: ListVariablesCommandOutput;
  };
};
}
