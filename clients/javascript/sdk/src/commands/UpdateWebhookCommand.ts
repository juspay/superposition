// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  UpdateWebhookInput,
  WebhookResponse,
} from "../models/models_0";
import {
  de_UpdateWebhookCommand,
  se_UpdateWebhookCommand,
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
 * The input for {@link UpdateWebhookCommand}.
 */
export interface UpdateWebhookCommandInput extends UpdateWebhookInput {}
/**
 * @public
 *
 * The output of {@link UpdateWebhookCommand}.
 */
export interface UpdateWebhookCommandOutput extends WebhookResponse, __MetadataBearer {}

/**
 * Updates an existing webhook config, allowing modification of URL, events, headers, and other webhook properties.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, UpdateWebhookCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, UpdateWebhookCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // UpdateWebhookInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   name: "STRING_VALUE", // required
 *   description: "STRING_VALUE", // required
 *   enabled: true || false, // required
 *   url: "STRING_VALUE", // required
 *   method: "GET" || "POST" || "PUT" || "PATCH" || "DELETE" || "HEAD", // required
 *   version: "V1",
 *   custom_headers: { // Object
 *     "<keys>": "DOCUMENT_VALUE",
 *   },
 *   events: [ // Events // required
 *     "STRING_VALUE",
 *   ],
 *   change_reason: "STRING_VALUE", // required
 * };
 * const command = new UpdateWebhookCommand(input);
 * const response = await client.send(command);
 * // { // WebhookResponse
 * //   name: "STRING_VALUE", // required
 * //   description: "STRING_VALUE", // required
 * //   enabled: true || false, // required
 * //   url: "STRING_VALUE", // required
 * //   method: "GET" || "POST" || "PUT" || "PATCH" || "DELETE" || "HEAD", // required
 * //   version: "V1", // required
 * //   custom_headers: { // Object
 * //     "<keys>": "DOCUMENT_VALUE",
 * //   },
 * //   events: [ // Events // required
 * //     "STRING_VALUE",
 * //   ],
 * //   max_retries: Number("int"), // required
 * //   last_triggered_at: new Date("TIMESTAMP"),
 * //   change_reason: "STRING_VALUE", // required
 * //   created_by: "STRING_VALUE", // required
 * //   created_at: new Date("TIMESTAMP"), // required
 * //   last_modified_by: "STRING_VALUE", // required
 * //   last_modified_at: new Date("TIMESTAMP"), // required
 * // };
 *
 * ```
 *
 * @param UpdateWebhookCommandInput - {@link UpdateWebhookCommandInput}
 * @returns {@link UpdateWebhookCommandOutput}
 * @see {@link UpdateWebhookCommandInput} for command's `input` shape.
 * @see {@link UpdateWebhookCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link WebhookNotFound} (client fault)
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class UpdateWebhookCommand extends $Command.classBuilder<UpdateWebhookCommandInput, UpdateWebhookCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "UpdateWebhook", {

  })
  .n("SuperpositionClient", "UpdateWebhookCommand")
  .f(void 0, void 0)
  .ser(se_UpdateWebhookCommand)
  .de(de_UpdateWebhookCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: UpdateWebhookInput;
      output: WebhookResponse;
  };
  sdk: {
      input: UpdateWebhookCommandInput;
      output: UpdateWebhookCommandOutput;
  };
};
}
