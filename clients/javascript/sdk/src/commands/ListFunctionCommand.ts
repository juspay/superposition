// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ListFunctionInput,
  ListFunctionOutput,
} from "../models/models_0";
import {
  de_ListFunctionCommand,
  se_ListFunctionCommand,
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
 * The input for {@link ListFunctionCommand}.
 */
export interface ListFunctionCommandInput extends ListFunctionInput {}
/**
 * @public
 *
 * The output of {@link ListFunctionCommand}.
 */
export interface ListFunctionCommandOutput extends ListFunctionOutput, __MetadataBearer {}

/**
 * Retrieves a paginated list of all functions in the workspace with their basic information and current status.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListFunctionCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListFunctionCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListFunctionInput
 *   count: Number("int"),
 *   page: Number("int"),
 *   all: true || false,
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 * };
 * const command = new ListFunctionCommand(input);
 * const response = await client.send(command);
 * // { // ListFunctionOutput
 * //   total_pages: Number("int"),
 * //   total_items: Number("int"),
 * //   data: [ // FunctionListResponse
 * //     { // FunctionResponse
 * //       function_name: "STRING_VALUE", // required
 * //       published_code: "STRING_VALUE",
 * //       draft_code: "STRING_VALUE", // required
 * //       published_runtime_version: "STRING_VALUE",
 * //       draft_runtime_version: "STRING_VALUE", // required
 * //       published_at: new Date("TIMESTAMP"),
 * //       draft_edited_at: new Date("TIMESTAMP"), // required
 * //       published_by: "STRING_VALUE",
 * //       draft_edited_by: "STRING_VALUE", // required
 * //       last_modified_at: new Date("TIMESTAMP"), // required
 * //       last_modified_by: "STRING_VALUE", // required
 * //       change_reason: "STRING_VALUE", // required
 * //       description: "STRING_VALUE", // required
 * //       function_type: "VALIDATION" || "AUTOCOMPLETE", // required
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param ListFunctionCommandInput - {@link ListFunctionCommandInput}
 * @returns {@link ListFunctionCommandOutput}
 * @see {@link ListFunctionCommandInput} for command's `input` shape.
 * @see {@link ListFunctionCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class ListFunctionCommand extends $Command.classBuilder<ListFunctionCommandInput, ListFunctionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "ListFunction", {

  })
  .n("SuperpositionClient", "ListFunctionCommand")
  .f(void 0, void 0)
  .ser(se_ListFunctionCommand)
  .de(de_ListFunctionCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: ListFunctionInput;
      output: ListFunctionOutput;
  };
  sdk: {
      input: ListFunctionCommandInput;
      output: ListFunctionCommandOutput;
  };
};
}
