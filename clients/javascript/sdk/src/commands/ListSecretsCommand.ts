// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ListSecretsInput,
  ListSecretsOutput,
} from "../models/models_0";
import {
  de_ListSecretsCommand,
  se_ListSecretsCommand,
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
 * The input for {@link ListSecretsCommand}.
 */
export interface ListSecretsCommandInput extends ListSecretsInput {}
/**
 * @public
 *
 * The output of {@link ListSecretsCommand}.
 */
export interface ListSecretsCommandOutput extends ListSecretsOutput, __MetadataBearer {}

/**
 * Retrieves a paginated list of all secrets in the workspace with optional filtering and sorting. All secret values are masked.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListSecretsCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListSecretsCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListSecretsInput
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
 * const command = new ListSecretsCommand(input);
 * const response = await client.send(command);
 * // { // ListSecretsOutput
 * //   total_pages: Number("int"), // required
 * //   total_items: Number("int"), // required
 * //   data: [ // SecretList // required
 * //     { // SecretResponse
 * //       name: "STRING_VALUE", // required
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
 * @param ListSecretsCommandInput - {@link ListSecretsCommandInput}
 * @returns {@link ListSecretsCommandOutput}
 * @see {@link ListSecretsCommandInput} for command's `input` shape.
 * @see {@link ListSecretsCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class ListSecretsCommand extends $Command.classBuilder<ListSecretsCommandInput, ListSecretsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "ListSecrets", {

  })
  .n("SuperpositionClient", "ListSecretsCommand")
  .f(void 0, void 0)
  .ser(se_ListSecretsCommand)
  .de(de_ListSecretsCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: ListSecretsInput;
      output: ListSecretsOutput;
  };
  sdk: {
      input: ListSecretsCommandInput;
      output: ListSecretsCommandOutput;
  };
};
}
