import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { CreateDefaultConfigInput, DefaultConfigFull } from "../models/models_0";
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
 * The input for {@link CreateDefaultConfigCommand}.
 */
export interface CreateDefaultConfigCommandInput extends CreateDefaultConfigInput {
}
/**
 * @public
 *
 * The output of {@link CreateDefaultConfigCommand}.
 */
export interface CreateDefaultConfigCommandOutput extends DefaultConfigFull, __MetadataBearer {
}
declare const CreateDefaultConfigCommand_base: {
    new (input: CreateDefaultConfigCommandInput): import("@smithy/smithy-client").CommandImpl<CreateDefaultConfigCommandInput, CreateDefaultConfigCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: CreateDefaultConfigCommandInput): import("@smithy/smithy-client").CommandImpl<CreateDefaultConfigCommandInput, CreateDefaultConfigCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, CreateDefaultConfigCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, CreateDefaultConfigCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // CreateDefaultConfigInput
 *   key: "STRING_VALUE", // required
 *   value: "DOCUMENT_VALUE", // required
 *   schema: "DOCUMENT_VALUE", // required
 *   description: "STRING_VALUE", // required
 *   change_reason: "STRING_VALUE", // required
 *   function_name: "STRING_VALUE",
 *   autocomplete_function_name: "STRING_VALUE",
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 * };
 * const command = new CreateDefaultConfigCommand(input);
 * const response = await client.send(command);
 * // { // DefaultConfigFull
 * //   key: "STRING_VALUE", // required
 * //   value: "DOCUMENT_VALUE", // required
 * //   schema: "DOCUMENT_VALUE", // required
 * //   description: "STRING_VALUE", // required
 * //   change_reason: "STRING_VALUE", // required
 * //   function_name: "STRING_VALUE",
 * //   autocomplete_function_name: "STRING_VALUE",
 * //   created_at: new Date("TIMESTAMP"), // required
 * //   created_by: "STRING_VALUE", // required
 * //   last_modified_at: new Date("TIMESTAMP"), // required
 * //   last_modified_by: "STRING_VALUE", // required
 * // };
 *
 * ```
 *
 * @param CreateDefaultConfigCommandInput - {@link CreateDefaultConfigCommandInput}
 * @returns {@link CreateDefaultConfigCommandOutput}
 * @see {@link CreateDefaultConfigCommandInput} for command's `input` shape.
 * @see {@link CreateDefaultConfigCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class CreateDefaultConfigCommand extends CreateDefaultConfigCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: CreateDefaultConfigInput;
            output: DefaultConfigFull;
        };
        sdk: {
            input: CreateDefaultConfigCommandInput;
            output: CreateDefaultConfigCommandOutput;
        };
    };
}
