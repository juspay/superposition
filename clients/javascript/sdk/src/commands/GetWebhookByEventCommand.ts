// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  GetWebhookByEventInput,
  WebhookResponse,
} from "../models/models_0";
import {
  de_GetWebhookByEventCommand,
  se_GetWebhookByEventCommand,
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
 * The input for {@link GetWebhookByEventCommand}.
 */
export interface GetWebhookByEventCommandInput extends GetWebhookByEventInput {}
/**
 * @public
 *
 * The output of {@link GetWebhookByEventCommand}.
 */
export interface GetWebhookByEventCommandOutput extends WebhookResponse, __MetadataBearer {}

/**
 * Retrieves a webhook configuration based on a specific event type, allowing users to find which webhook is set to trigger for that event.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetWebhookByEventCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetWebhookByEventCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetWebhookByEventInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   event: "STRING_VALUE", // required
 * };
 * const command = new GetWebhookByEventCommand(input);
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
 * @param GetWebhookByEventCommandInput - {@link GetWebhookByEventCommandInput}
 * @returns {@link GetWebhookByEventCommandOutput}
 * @see {@link GetWebhookByEventCommandInput} for command's `input` shape.
 * @see {@link GetWebhookByEventCommandOutput} for command's `response` shape.
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
export class GetWebhookByEventCommand extends $Command.classBuilder<GetWebhookByEventCommandInput, GetWebhookByEventCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetWebhookByEvent", {

  })
  .n("SuperpositionClient", "GetWebhookByEventCommand")
  .f(void 0, void 0)
  .ser(se_GetWebhookByEventCommand)
  .de(de_GetWebhookByEventCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetWebhookByEventInput;
      output: WebhookResponse;
  };
  sdk: {
      input: GetWebhookByEventCommandInput;
      output: GetWebhookByEventCommandOutput;
  };
};
}
