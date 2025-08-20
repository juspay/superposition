// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  GetWebhookInput,
  WebhookResponse,
} from "../models/models_0";
import {
  de_GetWebhookCommand,
  se_GetWebhookCommand,
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
 * The input for {@link GetWebhookCommand}.
 */
export interface GetWebhookCommandInput extends GetWebhookInput {}
/**
 * @public
 *
 * The output of {@link GetWebhookCommand}.
 */
export interface GetWebhookCommandOutput extends WebhookResponse, __MetadataBearer {}

/**
 * Retrieves detailed information about a specific webhook config, including its events, headers, and trigger history.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetWebhookCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetWebhookCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetWebhookInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   name: "STRING_VALUE", // required
 * };
 * const command = new GetWebhookCommand(input);
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
 * @param GetWebhookCommandInput - {@link GetWebhookCommandInput}
 * @returns {@link GetWebhookCommandOutput}
 * @see {@link GetWebhookCommandInput} for command's `input` shape.
 * @see {@link GetWebhookCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class GetWebhookCommand extends $Command.classBuilder<GetWebhookCommandInput, GetWebhookCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetWebhook", {

  })
  .n("SuperpositionClient", "GetWebhookCommand")
  .f(void 0, void 0)
  .ser(se_GetWebhookCommand)
  .de(de_GetWebhookCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetWebhookInput;
      output: WebhookResponse;
  };
  sdk: {
      input: GetWebhookCommandInput;
      output: GetWebhookCommandOutput;
  };
};
}
