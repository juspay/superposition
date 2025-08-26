// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ListWebhookInput,
  WebhookListResponse,
} from "../models/models_0";
import {
  de_ListWebhookCommand,
  se_ListWebhookCommand,
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
 * The input for {@link ListWebhookCommand}.
 */
export interface ListWebhookCommandInput extends ListWebhookInput {}
/**
 * @public
 *
 * The output of {@link ListWebhookCommand}.
 */
export interface ListWebhookCommandOutput extends WebhookListResponse, __MetadataBearer {}

/**
 * Retrieves a paginated list of all webhook configs in the workspace, including their status and config details.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListWebhookCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListWebhookCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListWebhookInput
 *   count: Number("int"),
 *   page: Number("int"),
 *   all: true || false,
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 * };
 * const command = new ListWebhookCommand(input);
 * const response = await client.send(command);
 * // { // WebhookListResponse
 * //   total_pages: Number("long"), // required
 * //   total_items: Number("long"), // required
 * //   data: [ // WebhookList // required
 * //     { // WebhookResponse
 * //       name: "STRING_VALUE", // required
 * //       description: "STRING_VALUE", // required
 * //       enabled: true || false, // required
 * //       url: "STRING_VALUE", // required
 * //       method: "GET" || "POST" || "PUT" || "PATCH" || "DELETE" || "HEAD", // required
 * //       version: "V1", // required
 * //       custom_headers: { // Object
 * //         "<keys>": "DOCUMENT_VALUE",
 * //       },
 * //       events: [ // Events // required
 * //         "STRING_VALUE",
 * //       ],
 * //       max_retries: Number("int"), // required
 * //       last_triggered_at: new Date("TIMESTAMP"),
 * //       change_reason: "STRING_VALUE", // required
 * //       created_by: "STRING_VALUE", // required
 * //       created_at: new Date("TIMESTAMP"), // required
 * //       last_modified_by: "STRING_VALUE", // required
 * //       last_modified_at: new Date("TIMESTAMP"), // required
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param ListWebhookCommandInput - {@link ListWebhookCommandInput}
 * @returns {@link ListWebhookCommandOutput}
 * @see {@link ListWebhookCommandInput} for command's `input` shape.
 * @see {@link ListWebhookCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class ListWebhookCommand extends $Command.classBuilder<ListWebhookCommandInput, ListWebhookCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "ListWebhook", {

  })
  .n("SuperpositionClient", "ListWebhookCommand")
  .f(void 0, void 0)
  .ser(se_ListWebhookCommand)
  .de(de_ListWebhookCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: ListWebhookInput;
      output: WebhookListResponse;
  };
  sdk: {
      input: ListWebhookCommandInput;
      output: ListWebhookCommandOutput;
  };
};
}
