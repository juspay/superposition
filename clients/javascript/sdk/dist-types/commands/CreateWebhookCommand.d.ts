import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { CreateWebhookInput, WebhookResponse } from "../models/models_0";
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
export interface CreateWebhookCommandInput extends CreateWebhookInput {
}
/**
 * @public
 *
 * The output of {@link CreateWebhookCommand}.
 */
export interface CreateWebhookCommandOutput extends WebhookResponse, __MetadataBearer {
}
declare const CreateWebhookCommand_base: {
    new (input: CreateWebhookCommandInput): import("@smithy/smithy-client").CommandImpl<CreateWebhookCommandInput, CreateWebhookCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: CreateWebhookCommandInput): import("@smithy/smithy-client").CommandImpl<CreateWebhookCommandInput, CreateWebhookCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, CreateWebhookCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, CreateWebhookCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
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
 */
export declare class CreateWebhookCommand extends CreateWebhookCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
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
