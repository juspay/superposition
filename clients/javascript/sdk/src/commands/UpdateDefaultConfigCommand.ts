// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  DefaultConfigFull,
  UpdateDefaultConfigInput,
} from "../models/models_0";
import {
  de_UpdateDefaultConfigCommand,
  se_UpdateDefaultConfigCommand,
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
 * The input for {@link UpdateDefaultConfigCommand}.
 */
export interface UpdateDefaultConfigCommandInput extends UpdateDefaultConfigInput {}
/**
 * @public
 *
 * The output of {@link UpdateDefaultConfigCommand}.
 */
export interface UpdateDefaultConfigCommandOutput extends DefaultConfigFull, __MetadataBearer {}

/**
 * Updates an existing default config entry. Allows modification of value, schema, function mappings, and description while preserving the key identifier.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, UpdateDefaultConfigCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, UpdateDefaultConfigCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // UpdateDefaultConfigInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   key: "STRING_VALUE", // required
 *   change_reason: "STRING_VALUE", // required
 *   value: "DOCUMENT_VALUE",
 *   schema: "DOCUMENT_VALUE",
 *   function_name: "STRING_VALUE",
 *   description: "STRING_VALUE",
 *   autocomplete_function_name: "STRING_VALUE",
 * };
 * const command = new UpdateDefaultConfigCommand(input);
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
 * @param UpdateDefaultConfigCommandInput - {@link UpdateDefaultConfigCommandInput}
 * @returns {@link UpdateDefaultConfigCommandOutput}
 * @see {@link UpdateDefaultConfigCommandInput} for command's `input` shape.
 * @see {@link UpdateDefaultConfigCommandOutput} for command's `response` shape.
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
export class UpdateDefaultConfigCommand extends $Command.classBuilder<UpdateDefaultConfigCommandInput, UpdateDefaultConfigCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "UpdateDefaultConfig", {

  })
  .n("SuperpositionClient", "UpdateDefaultConfigCommand")
  .f(void 0, void 0)
  .ser(se_UpdateDefaultConfigCommand)
  .de(de_UpdateDefaultConfigCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: UpdateDefaultConfigInput;
      output: DefaultConfigFull;
  };
  sdk: {
      input: UpdateDefaultConfigCommandInput;
      output: UpdateDefaultConfigCommandOutput;
  };
};
}
