// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import { RotateMasterEncryptionKeyOutput } from "../models/models_0";
import {
  de_RotateMasterEncryptionKeyCommand,
  se_RotateMasterEncryptionKeyCommand,
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
 * The input for {@link RotateMasterEncryptionKeyCommand}.
 */
export interface RotateMasterEncryptionKeyCommandInput {}
/**
 * @public
 *
 * The output of {@link RotateMasterEncryptionKeyCommand}.
 */
export interface RotateMasterEncryptionKeyCommandOutput extends RotateMasterEncryptionKeyOutput, __MetadataBearer {}

/**
 * Rotates the master encryption key across all workspaces
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, RotateMasterEncryptionKeyCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, RotateMasterEncryptionKeyCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = {};
 * const command = new RotateMasterEncryptionKeyCommand(input);
 * const response = await client.send(command);
 * // { // RotateMasterEncryptionKeyOutput
 * //   workspaces_rotated: Number("long"), // required
 * //   total_secrets_re_encrypted: Number("long"), // required
 * // };
 *
 * ```
 *
 * @param RotateMasterEncryptionKeyCommandInput - {@link RotateMasterEncryptionKeyCommandInput}
 * @returns {@link RotateMasterEncryptionKeyCommandOutput}
 * @see {@link RotateMasterEncryptionKeyCommandInput} for command's `input` shape.
 * @see {@link RotateMasterEncryptionKeyCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class RotateMasterEncryptionKeyCommand extends $Command.classBuilder<RotateMasterEncryptionKeyCommandInput, RotateMasterEncryptionKeyCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "RotateMasterEncryptionKey", {

  })
  .n("SuperpositionClient", "RotateMasterEncryptionKeyCommand")
  .f(void 0, void 0)
  .ser(se_RotateMasterEncryptionKeyCommand)
  .de(de_RotateMasterEncryptionKeyCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: {};
      output: RotateMasterEncryptionKeyOutput;
  };
  sdk: {
      input: RotateMasterEncryptionKeyCommandInput;
      output: RotateMasterEncryptionKeyCommandOutput;
  };
};
}
