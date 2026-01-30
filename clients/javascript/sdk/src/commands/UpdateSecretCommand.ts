// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  SecretResponse,
  UpdateSecretInput,
} from "../models/models_0";
import {
  de_UpdateSecretCommand,
  se_UpdateSecretCommand,
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
 * The input for {@link UpdateSecretCommand}.
 */
export interface UpdateSecretCommandInput extends UpdateSecretInput {}
/**
 * @public
 *
 * The output of {@link UpdateSecretCommand}.
 */
export interface UpdateSecretCommandOutput extends SecretResponse, __MetadataBearer {}

/**
 * Updates an existing secret's value or description. The value is re-encrypted with the current workspace encryption key. Returns masked value.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, UpdateSecretCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, UpdateSecretCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // UpdateSecretInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   name: "STRING_VALUE", // required
 *   value: "STRING_VALUE",
 *   description: "STRING_VALUE",
 *   change_reason: "STRING_VALUE", // required
 * };
 * const command = new UpdateSecretCommand(input);
 * const response = await client.send(command);
 * // { // SecretResponse
 * //   name: "STRING_VALUE", // required
 * //   description: "STRING_VALUE", // required
 * //   change_reason: "STRING_VALUE", // required
 * //   created_by: "STRING_VALUE", // required
 * //   created_at: new Date("TIMESTAMP"), // required
 * //   last_modified_by: "STRING_VALUE", // required
 * //   last_modified_at: new Date("TIMESTAMP"), // required
 * // };
 *
 * ```
 *
 * @param UpdateSecretCommandInput - {@link UpdateSecretCommandInput}
 * @returns {@link UpdateSecretCommandOutput}
 * @see {@link UpdateSecretCommandInput} for command's `input` shape.
 * @see {@link UpdateSecretCommandOutput} for command's `response` shape.
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
export class UpdateSecretCommand extends $Command.classBuilder<UpdateSecretCommandInput, UpdateSecretCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "UpdateSecret", {

  })
  .n("SuperpositionClient", "UpdateSecretCommand")
  .f(void 0, void 0)
  .ser(se_UpdateSecretCommand)
  .de(de_UpdateSecretCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: UpdateSecretInput;
      output: SecretResponse;
  };
  sdk: {
      input: UpdateSecretCommandInput;
      output: UpdateSecretCommandOutput;
  };
};
}
