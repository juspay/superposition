import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { FunctionResponse, PublishInput } from "../models/models_0";
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
 * The input for {@link PublishCommand}.
 */
export interface PublishCommandInput extends PublishInput {
}
/**
 * @public
 *
 * The output of {@link PublishCommand}.
 */
export interface PublishCommandOutput extends FunctionResponse, __MetadataBearer {
}
declare const PublishCommand_base: {
    new (input: PublishCommandInput): import("@smithy/smithy-client").CommandImpl<PublishCommandInput, PublishCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: PublishCommandInput): import("@smithy/smithy-client").CommandImpl<PublishCommandInput, PublishCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, PublishCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, PublishCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // PublishInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   function_name: "STRING_VALUE", // required
 *   change_reason: "STRING_VALUE", // required
 * };
 * const command = new PublishCommand(input);
 * const response = await client.send(command);
 * // { // FunctionResponse
 * //   function_name: "STRING_VALUE", // required
 * //   published_code: "STRING_VALUE",
 * //   draft_code: "STRING_VALUE", // required
 * //   published_runtime_version: "STRING_VALUE",
 * //   draft_runtime_version: "STRING_VALUE", // required
 * //   published_at: new Date("TIMESTAMP"),
 * //   draft_edited_at: new Date("TIMESTAMP"), // required
 * //   published_by: "STRING_VALUE",
 * //   draft_edited_by: "STRING_VALUE", // required
 * //   last_modified_at: new Date("TIMESTAMP"), // required
 * //   last_modified_by: "STRING_VALUE", // required
 * //   change_reason: "STRING_VALUE", // required
 * //   description: "STRING_VALUE", // required
 * //   function_type: "VALIDATION" || "AUTOCOMPLETE", // required
 * // };
 *
 * ```
 *
 * @param PublishCommandInput - {@link PublishCommandInput}
 * @returns {@link PublishCommandOutput}
 * @see {@link PublishCommandInput} for command's `input` shape.
 * @see {@link PublishCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link FunctionNotFound} (client fault)
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class PublishCommand extends PublishCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: PublishInput;
            output: FunctionResponse;
        };
        sdk: {
            input: PublishCommandInput;
            output: PublishCommandOutput;
        };
    };
}
