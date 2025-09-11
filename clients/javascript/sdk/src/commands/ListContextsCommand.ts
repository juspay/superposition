// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ListContextsInput,
  ListContextsOutput,
} from "../models/models_0";
import {
  de_ListContextsCommand,
  se_ListContextsCommand,
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
 * The input for {@link ListContextsCommand}.
 */
export interface ListContextsCommandInput extends ListContextsInput {}
/**
 * @public
 *
 * The output of {@link ListContextsCommand}.
 */
export interface ListContextsCommandOutput extends ListContextsOutput, __MetadataBearer {}

/**
 * Retrieves a paginated list of contexts with support for filtering by creation date, modification date, weight, and other criteria.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListContextsCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListContextsCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListContextsInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   page: Number("int"),
 *   count: Number("int"),
 *   all: true || false,
 *   prefix: "STRING_VALUE",
 *   sort_on: "last_modified_at" || "created_at" || "weight",
 *   sort_by: "desc" || "asc",
 *   created_by: "STRING_VALUE",
 *   last_modified_by: "STRING_VALUE",
 *   plaintext: "STRING_VALUE",
 * };
 * const command = new ListContextsCommand(input);
 * const response = await client.send(command);
 * // { // ListContextsOutput
 * //   total_pages: Number("int"),
 * //   total_items: Number("int"),
 * //   data: [ // ListContextOut
 * //     { // ContextResponse
 * //       id: "STRING_VALUE", // required
 * //       value: { // Condition
 * //         "<keys>": "DOCUMENT_VALUE",
 * //       },
 * //       override: { // Overrides
 * //         "<keys>": "DOCUMENT_VALUE",
 * //       },
 * //       override_id: "STRING_VALUE",
 * //       weight: "STRING_VALUE",
 * //       description: "STRING_VALUE",
 * //       change_reason: "STRING_VALUE",
 * //       created_at: new Date("TIMESTAMP"),
 * //       created_by: "STRING_VALUE",
 * //       last_modified_at: new Date("TIMESTAMP"),
 * //       last_modified_by: "STRING_VALUE",
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param ListContextsCommandInput - {@link ListContextsCommandInput}
 * @returns {@link ListContextsCommandOutput}
 * @see {@link ListContextsCommandInput} for command's `input` shape.
 * @see {@link ListContextsCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class ListContextsCommand extends $Command.classBuilder<ListContextsCommandInput, ListContextsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "ListContexts", {

  })
  .n("SuperpositionClient", "ListContextsCommand")
  .f(void 0, void 0)
  .ser(se_ListContextsCommand)
  .de(de_ListContextsCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: ListContextsInput;
      output: ListContextsOutput;
  };
  sdk: {
      input: ListContextsCommandInput;
      output: ListContextsCommandOutput;
  };
};
}
