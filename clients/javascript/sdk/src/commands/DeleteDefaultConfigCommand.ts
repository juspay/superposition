// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  DeleteDefaultConfigInput,
  DeleteDefaultConfigOutput,
} from "../models/models_0";
import {
  de_DeleteDefaultConfigCommand,
  se_DeleteDefaultConfigCommand,
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
 * The input for {@link DeleteDefaultConfigCommand}.
 */
export interface DeleteDefaultConfigCommandInput extends DeleteDefaultConfigInput {}
/**
 * @public
 *
 * The output of {@link DeleteDefaultConfigCommand}.
 */
export interface DeleteDefaultConfigCommandOutput extends DeleteDefaultConfigOutput, __MetadataBearer {}

/**
 * Permanently removes a default config entry from the workspace. This operation cannot be performed if it affects config resolution for contexts that rely on this fallback value.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, DeleteDefaultConfigCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, DeleteDefaultConfigCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // DeleteDefaultConfigInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   key: "STRING_VALUE", // required
 * };
 * const command = new DeleteDefaultConfigCommand(input);
 * const response = await client.send(command);
 * // {};
 *
 * ```
 *
 * @param DeleteDefaultConfigCommandInput - {@link DeleteDefaultConfigCommandInput}
 * @returns {@link DeleteDefaultConfigCommandOutput}
 * @see {@link DeleteDefaultConfigCommandInput} for command's `input` shape.
 * @see {@link DeleteDefaultConfigCommandOutput} for command's `response` shape.
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
export class DeleteDefaultConfigCommand extends $Command.classBuilder<DeleteDefaultConfigCommandInput, DeleteDefaultConfigCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "DeleteDefaultConfig", {

  })
  .n("SuperpositionClient", "DeleteDefaultConfigCommand")
  .f(void 0, void 0)
  .ser(se_DeleteDefaultConfigCommand)
  .de(de_DeleteDefaultConfigCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: DeleteDefaultConfigInput;
      output: {};
  };
  sdk: {
      input: DeleteDefaultConfigCommandInput;
      output: DeleteDefaultConfigCommandOutput;
  };
};
}
