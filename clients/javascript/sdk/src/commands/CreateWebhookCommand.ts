// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  CreateWebhookInput,
  WebhookResponse,
} from "../models/models_0";
import {
  de_CreateWebhookCommand,
  se_CreateWebhookCommand,
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
 * The input for {@link CreateWebhookCommand}.
 */
export interface CreateWebhookCommandInput extends CreateWebhookInput {}
/**
 * @public
 *
 * The output of {@link CreateWebhookCommand}.
 */
export interface CreateWebhookCommandOutput extends WebhookResponse, __MetadataBearer {}

/**
 * Creates a new webhook config to receive HTTP notifications when specified events occur in the system.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, CreateWebhookCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, CreateWebhookCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // CreateWebhookInput
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
 * const command = new CreateWebhookCommand(input);
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
 * @param CreateWebhookCommandInput - {@link CreateWebhookCommandInput}
 * @returns {@link CreateWebhookCommandOutput}
 * @see {@link CreateWebhookCommandInput} for command's `input` shape.
 * @see {@link CreateWebhookCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class CreateWebhookCommand extends $Command.classBuilder<CreateWebhookCommandInput, CreateWebhookCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "CreateWebhook", {

  })
  .n("SuperpositionClient", "CreateWebhookCommand")
  .f(void 0, void 0)
  .ser(se_CreateWebhookCommand)
  .de(de_CreateWebhookCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: CreateWebhookInput;
      output: WebhookResponse;
  };
  sdk: {
      input: CreateWebhookCommandInput;
      output: CreateWebhookCommandOutput;
  };
};
}
