import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { ListWebhookInput, WebhookListResponse } from "../models/models_0";
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
export interface ListWebhookCommandInput extends ListWebhookInput {
}
/**
 * @public
 *
 * The output of {@link ListWebhookCommand}.
 */
export interface ListWebhookCommandOutput extends WebhookListResponse, __MetadataBearer {
}
declare const ListWebhookCommand_base: {
    new (input: ListWebhookCommandInput): import("@smithy/smithy-client").CommandImpl<ListWebhookCommandInput, ListWebhookCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: ListWebhookCommandInput): import("@smithy/smithy-client").CommandImpl<ListWebhookCommandInput, ListWebhookCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListWebhookCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListWebhookCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
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
 */
export declare class ListWebhookCommand extends ListWebhookCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
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
