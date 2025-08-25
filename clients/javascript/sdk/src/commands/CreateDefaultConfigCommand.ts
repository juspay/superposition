// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  CreateDefaultConfigInput,
  DefaultConfigFull,
} from "../models/models_0";
import {
  de_CreateDefaultConfigCommand,
  se_CreateDefaultConfigCommand,
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
 * The input for {@link CreateDefaultConfigCommand}.
 */
export interface CreateDefaultConfigCommandInput extends CreateDefaultConfigInput {}
/**
 * @public
 *
 * The output of {@link CreateDefaultConfigCommand}.
 */
export interface CreateDefaultConfigCommandOutput extends DefaultConfigFull, __MetadataBearer {}

/**
 * Creates a new default config entry with specified key, value, schema, and metadata. Default configs serve as fallback values when no specific context matches.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, CreateDefaultConfigCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, CreateDefaultConfigCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // CreateDefaultConfigInput
 *   key: "STRING_VALUE", // required
 *   value: "DOCUMENT_VALUE", // required
 *   schema: "DOCUMENT_VALUE", // required
 *   description: "STRING_VALUE", // required
 *   change_reason: "STRING_VALUE", // required
 *   function_name: "STRING_VALUE",
 *   autocomplete_function_name: "STRING_VALUE",
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 * };
 * const command = new CreateDefaultConfigCommand(input);
 * const response = await client.send(command);
 * // { // DefaultConfigFull
 * //   key: "STRING_VALUE", // required
 * //   value: "DOCUMENT_VALUE", // required
 * //   schema: "DOCUMENT_VALUE", // required
 * //   description: "STRING_VALUE", // required
 * //   change_reason: "STRING_VALUE", // required
 * //   function_name: "STRING_VALUE",
 * //   autocomplete_function_name: "STRING_VALUE",
 * //   created_at: new Date("TIMESTAMP"), // required
 * //   created_by: "STRING_VALUE", // required
 * //   last_modified_at: new Date("TIMESTAMP"), // required
 * //   last_modified_by: "STRING_VALUE", // required
 * // };
 *
 * ```
 *
 * @param CreateDefaultConfigCommandInput - {@link CreateDefaultConfigCommandInput}
 * @returns {@link CreateDefaultConfigCommandOutput}
 * @see {@link CreateDefaultConfigCommandInput} for command's `input` shape.
 * @see {@link CreateDefaultConfigCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class CreateDefaultConfigCommand extends $Command.classBuilder<CreateDefaultConfigCommandInput, CreateDefaultConfigCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "CreateDefaultConfig", {

  })
  .n("SuperpositionClient", "CreateDefaultConfigCommand")
  .f(void 0, void 0)
  .ser(se_CreateDefaultConfigCommand)
  .de(de_CreateDefaultConfigCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: CreateDefaultConfigInput;
      output: DefaultConfigFull;
  };
  sdk: {
      input: CreateDefaultConfigCommandInput;
      output: CreateDefaultConfigCommandOutput;
  };
};
}
