// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  CreateSecretInput,
  SecretResponse,
} from "../models/models_0";
import {
  de_CreateSecretCommand,
  se_CreateSecretCommand,
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
 * The input for {@link CreateSecretCommand}.
 */
export interface CreateSecretCommandInput extends CreateSecretInput {}
/**
 * @public
 *
 * The output of {@link CreateSecretCommand}.
 */
export interface CreateSecretCommandOutput extends SecretResponse, __MetadataBearer {}

/**
 * Creates a new encrypted secret with the specified name and value. The secret is encrypted with the workspace's current encryption key. Secret values are never returned in responses for security.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, CreateSecretCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, CreateSecretCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // CreateSecretInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   name: "STRING_VALUE", // required
 *   value: "STRING_VALUE", // required
 *   description: "STRING_VALUE", // required
 *   change_reason: "STRING_VALUE", // required
 * };
 * const command = new CreateSecretCommand(input);
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
 * @param CreateSecretCommandInput - {@link CreateSecretCommandInput}
 * @returns {@link CreateSecretCommandOutput}
 * @see {@link CreateSecretCommandInput} for command's `input` shape.
 * @see {@link CreateSecretCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class CreateSecretCommand extends $Command.classBuilder<CreateSecretCommandInput, CreateSecretCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "CreateSecret", {

  })
  .n("SuperpositionClient", "CreateSecretCommand")
  .f(void 0, void 0)
  .ser(se_CreateSecretCommand)
  .de(de_CreateSecretCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: CreateSecretInput;
      output: SecretResponse;
  };
  sdk: {
      input: CreateSecretCommandInput;
      output: CreateSecretCommandOutput;
  };
};
}
