// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import { DeleteWebhookInput } from "../models/models_0";
import {
  de_DeleteWebhookCommand,
  se_DeleteWebhookCommand,
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
 * The input for {@link DeleteWebhookCommand}.
 */
export interface DeleteWebhookCommandInput extends DeleteWebhookInput {}
/**
 * @public
 *
 * The output of {@link DeleteWebhookCommand}.
 */
export interface DeleteWebhookCommandOutput extends __MetadataBearer {}

/**
 * Permanently removes a webhook config from the workspace, stopping all future event notifications to that endpoint.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, DeleteWebhookCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, DeleteWebhookCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // DeleteWebhookInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   name: "STRING_VALUE", // required
 * };
 * const command = new DeleteWebhookCommand(input);
 * const response = await client.send(command);
 * // {};
 *
 * ```
 *
 * @param DeleteWebhookCommandInput - {@link DeleteWebhookCommandInput}
 * @returns {@link DeleteWebhookCommandOutput}
 * @see {@link DeleteWebhookCommandInput} for command's `input` shape.
 * @see {@link DeleteWebhookCommandOutput} for command's `response` shape.
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
export class DeleteWebhookCommand extends $Command.classBuilder<DeleteWebhookCommandInput, DeleteWebhookCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "DeleteWebhook", {

  })
  .n("SuperpositionClient", "DeleteWebhookCommand")
  .f(void 0, void 0)
  .ser(se_DeleteWebhookCommand)
  .de(de_DeleteWebhookCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: DeleteWebhookInput;
      output: {};
  };
  sdk: {
      input: DeleteWebhookCommandInput;
      output: DeleteWebhookCommandOutput;
  };
};
}
