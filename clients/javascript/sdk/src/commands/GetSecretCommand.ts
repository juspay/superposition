// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  GetSecretInput,
  SecretResponse,
} from "../models/models_0";
import {
  de_GetSecretCommand,
  se_GetSecretCommand,
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
 * The input for {@link GetSecretCommand}.
 */
export interface GetSecretCommandInput extends GetSecretInput {}
/**
 * @public
 *
 * The output of {@link GetSecretCommand}.
 */
export interface GetSecretCommandOutput extends SecretResponse, __MetadataBearer {}

/**
 * Retrieves detailed information about a specific secret by its name. The value is masked for security.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetSecretCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetSecretCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetSecretInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   name: "STRING_VALUE", // required
 * };
 * const command = new GetSecretCommand(input);
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
 * @param GetSecretCommandInput - {@link GetSecretCommandInput}
 * @returns {@link GetSecretCommandOutput}
 * @see {@link GetSecretCommandInput} for command's `input` shape.
 * @see {@link GetSecretCommandOutput} for command's `response` shape.
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
export class GetSecretCommand extends $Command.classBuilder<GetSecretCommandInput, GetSecretCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetSecret", {

  })
  .n("SuperpositionClient", "GetSecretCommand")
  .f(void 0, void 0)
  .ser(se_GetSecretCommand)
  .de(de_GetSecretCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetSecretInput;
      output: SecretResponse;
  };
  sdk: {
      input: GetSecretCommandInput;
      output: GetSecretCommandOutput;
  };
};
}
