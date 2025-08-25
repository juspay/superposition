// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ListDefaultConfigsInput,
  ListDefaultConfigsOutput,
} from "../models/models_0";
import {
  de_ListDefaultConfigsCommand,
  se_ListDefaultConfigsCommand,
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
 * The input for {@link ListDefaultConfigsCommand}.
 */
export interface ListDefaultConfigsCommandInput extends ListDefaultConfigsInput {}
/**
 * @public
 *
 * The output of {@link ListDefaultConfigsCommand}.
 */
export interface ListDefaultConfigsCommandOutput extends ListDefaultConfigsOutput, __MetadataBearer {}

/**
 * Retrieves a paginated list of all default config entries in the workspace, including their values, schemas, and metadata.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListDefaultConfigsCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListDefaultConfigsCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListDefaultConfigsInput
 *   count: Number("int"),
 *   page: Number("int"),
 *   all: true || false,
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 * };
 * const command = new ListDefaultConfigsCommand(input);
 * const response = await client.send(command);
 * // { // ListDefaultConfigsOutput
 * //   total_pages: Number("int"),
 * //   total_items: Number("int"),
 * //   data: [ // ListDefaultConfigOut
 * //     { // DefaultConfigFull
 * //       key: "STRING_VALUE", // required
 * //       value: "DOCUMENT_VALUE", // required
 * //       schema: "DOCUMENT_VALUE", // required
 * //       description: "STRING_VALUE", // required
 * //       change_reason: "STRING_VALUE", // required
 * //       function_name: "STRING_VALUE",
 * //       autocomplete_function_name: "STRING_VALUE",
 * //       created_at: new Date("TIMESTAMP"), // required
 * //       created_by: "STRING_VALUE", // required
 * //       last_modified_at: new Date("TIMESTAMP"), // required
 * //       last_modified_by: "STRING_VALUE", // required
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param ListDefaultConfigsCommandInput - {@link ListDefaultConfigsCommandInput}
 * @returns {@link ListDefaultConfigsCommandOutput}
 * @see {@link ListDefaultConfigsCommandInput} for command's `input` shape.
 * @see {@link ListDefaultConfigsCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link ResourceNotFound} (client fault)
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class ListDefaultConfigsCommand extends $Command.classBuilder<ListDefaultConfigsCommandInput, ListDefaultConfigsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "ListDefaultConfigs", {

  })
  .n("SuperpositionClient", "ListDefaultConfigsCommand")
  .f(void 0, void 0)
  .ser(se_ListDefaultConfigsCommand)
  .de(de_ListDefaultConfigsCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: ListDefaultConfigsInput;
      output: ListDefaultConfigsOutput;
  };
  sdk: {
      input: ListDefaultConfigsCommandInput;
      output: ListDefaultConfigsCommandOutput;
  };
};
}
