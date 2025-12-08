// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  DeleteSecretInput,
  SecretResponse,
} from "../models/models_0";
import {
  de_DeleteSecretCommand,
  se_DeleteSecretCommand,
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
 * The input for {@link DeleteSecretCommand}.
 */
export interface DeleteSecretCommandInput extends DeleteSecretInput {}
/**
 * @public
 *
 * The output of {@link DeleteSecretCommand}.
 */
export interface DeleteSecretCommandOutput extends SecretResponse, __MetadataBearer {}

/**
 * Permanently deletes a secret from the workspace. The encrypted value is removed and cannot be recovered.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, DeleteSecretCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, DeleteSecretCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // DeleteSecretInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   name: "STRING_VALUE", // required
 * };
 * const command = new DeleteSecretCommand(input);
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
 * @param DeleteSecretCommandInput - {@link DeleteSecretCommandInput}
 * @returns {@link DeleteSecretCommandOutput}
 * @see {@link DeleteSecretCommandInput} for command's `input` shape.
 * @see {@link DeleteSecretCommandOutput} for command's `response` shape.
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
export class DeleteSecretCommand extends $Command.classBuilder<DeleteSecretCommandInput, DeleteSecretCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "DeleteSecret", {

  })
  .n("SuperpositionClient", "DeleteSecretCommand")
  .f(void 0, void 0)
  .ser(se_DeleteSecretCommand)
  .de(de_DeleteSecretCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: DeleteSecretInput;
      output: SecretResponse;
  };
  sdk: {
      input: DeleteSecretCommandInput;
      output: DeleteSecretCommandOutput;
  };
};
}
