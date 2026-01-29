// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import { RotateMasterKeyResponse } from "../models/models_0";
import {
  de_RotateMasterKeyCommand,
  se_RotateMasterKeyCommand,
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
 * The input for {@link RotateMasterKeyCommand}.
 */
export interface RotateMasterKeyCommandInput {}
/**
 * @public
 *
 * The output of {@link RotateMasterKeyCommand}.
 */
export interface RotateMasterKeyCommandOutput extends RotateMasterKeyResponse, __MetadataBearer {}

/**
 * Rotates the master key encryption key across all workspaces
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, RotateMasterKeyCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, RotateMasterKeyCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = {};
 * const command = new RotateMasterKeyCommand(input);
 * const response = await client.send(command);
 * // { // RotateMasterKeyResponse
 * //   workspaces_rotated: Number("long"), // required
 * //   total_secrets_re_encrypted: Number("long"), // required
 * //   rotated_at: new Date("TIMESTAMP"), // required
 * //   new_master_key: "STRING_VALUE", // required
 * // };
 *
 * ```
 *
 * @param RotateMasterKeyCommandInput - {@link RotateMasterKeyCommandInput}
 * @returns {@link RotateMasterKeyCommandOutput}
 * @see {@link RotateMasterKeyCommandInput} for command's `input` shape.
 * @see {@link RotateMasterKeyCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class RotateMasterKeyCommand extends $Command.classBuilder<RotateMasterKeyCommandInput, RotateMasterKeyCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "RotateMasterKey", {

  })
  .n("SuperpositionClient", "RotateMasterKeyCommand")
  .f(void 0, void 0)
  .ser(se_RotateMasterKeyCommand)
  .de(de_RotateMasterKeyCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: {};
      output: RotateMasterKeyResponse;
  };
  sdk: {
      input: RotateMasterKeyCommandInput;
      output: RotateMasterKeyCommandOutput;
  };
};
}
