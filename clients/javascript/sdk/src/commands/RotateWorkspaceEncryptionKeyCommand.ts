// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  RotateWorkspaceEncryptionKeyOutput,
  WorkspaceSelectorRequest,
} from "../models/models_0";
import {
  de_RotateWorkspaceEncryptionKeyCommand,
  se_RotateWorkspaceEncryptionKeyCommand,
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
 * The input for {@link RotateWorkspaceEncryptionKeyCommand}.
 */
export interface RotateWorkspaceEncryptionKeyCommandInput extends WorkspaceSelectorRequest {}
/**
 * @public
 *
 * The output of {@link RotateWorkspaceEncryptionKeyCommand}.
 */
export interface RotateWorkspaceEncryptionKeyCommandOutput extends RotateWorkspaceEncryptionKeyOutput, __MetadataBearer {}

/**
 * Rotates the workspace encryption key. Generates a new encryption key and re-encrypts all secrets with the new key. This is a critical operation that should be done during low-traffic periods.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, RotateWorkspaceEncryptionKeyCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, RotateWorkspaceEncryptionKeyCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // WorkspaceSelectorRequest
 *   org_id: "STRING_VALUE", // required
 *   workspace_name: "STRING_VALUE", // required
 * };
 * const command = new RotateWorkspaceEncryptionKeyCommand(input);
 * const response = await client.send(command);
 * // { // RotateWorkspaceEncryptionKeyOutput
 * //   total_secrets_re_encrypted: Number("long"), // required
 * // };
 *
 * ```
 *
 * @param RotateWorkspaceEncryptionKeyCommandInput - {@link RotateWorkspaceEncryptionKeyCommandInput}
 * @returns {@link RotateWorkspaceEncryptionKeyCommandOutput}
 * @see {@link RotateWorkspaceEncryptionKeyCommandInput} for command's `input` shape.
 * @see {@link RotateWorkspaceEncryptionKeyCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class RotateWorkspaceEncryptionKeyCommand extends $Command.classBuilder<RotateWorkspaceEncryptionKeyCommandInput, RotateWorkspaceEncryptionKeyCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "RotateWorkspaceEncryptionKey", {

  })
  .n("SuperpositionClient", "RotateWorkspaceEncryptionKeyCommand")
  .f(void 0, void 0)
  .ser(se_RotateWorkspaceEncryptionKeyCommand)
  .de(de_RotateWorkspaceEncryptionKeyCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: WorkspaceSelectorRequest;
      output: RotateWorkspaceEncryptionKeyOutput;
  };
  sdk: {
      input: RotateWorkspaceEncryptionKeyCommandInput;
      output: RotateWorkspaceEncryptionKeyCommandOutput;
  };
};
}
