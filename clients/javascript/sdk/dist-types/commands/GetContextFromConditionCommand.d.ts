import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { ContextFull, GetContextFromConditionInput } from "../models/models_0";
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
 * The input for {@link GetContextFromConditionCommand}.
 */
export interface GetContextFromConditionCommandInput extends GetContextFromConditionInput {
}
/**
 * @public
 *
 * The output of {@link GetContextFromConditionCommand}.
 */
export interface GetContextFromConditionCommandOutput extends ContextFull, __MetadataBearer {
}
declare const GetContextFromConditionCommand_base: {
    new (input: GetContextFromConditionCommandInput): import("@smithy/smithy-client").CommandImpl<GetContextFromConditionCommandInput, GetContextFromConditionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: GetContextFromConditionCommandInput): import("@smithy/smithy-client").CommandImpl<GetContextFromConditionCommandInput, GetContextFromConditionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetContextFromConditionCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetContextFromConditionCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetContextFromConditionInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   context: "DOCUMENT_VALUE",
 * };
 * const command = new GetContextFromConditionCommand(input);
 * const response = await client.send(command);
 * // { // ContextFull
 * //   id: "STRING_VALUE", // required
 * //   value: { // Condition
 * //     "<keys>": "DOCUMENT_VALUE",
 * //   },
 * //   override: { // Overrides
 * //     "<keys>": "DOCUMENT_VALUE",
 * //   },
 * //   override_id: "STRING_VALUE",
 * //   weight: "STRING_VALUE",
 * //   description: "STRING_VALUE",
 * //   change_reason: "STRING_VALUE",
 * //   created_at: new Date("TIMESTAMP"),
 * //   created_by: "STRING_VALUE",
 * //   last_modified_at: new Date("TIMESTAMP"),
 * //   last_modified_by: "STRING_VALUE",
 * // };
 *
 * ```
 *
 * @param GetContextFromConditionCommandInput - {@link GetContextFromConditionCommandInput}
 * @returns {@link GetContextFromConditionCommandOutput}
 * @see {@link GetContextFromConditionCommandInput} for command's `input` shape.
 * @see {@link GetContextFromConditionCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link ResourceNotFound} (client fault)
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class GetContextFromConditionCommand extends GetContextFromConditionCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: GetContextFromConditionInput;
            output: ContextFull;
        };
        sdk: {
            input: GetContextFromConditionCommandInput;
            output: GetContextFromConditionCommandOutput;
        };
    };
}
