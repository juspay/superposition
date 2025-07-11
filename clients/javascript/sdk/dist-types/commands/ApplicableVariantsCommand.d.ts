import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { ApplicableVariantsInput, ApplicableVariantsOutput } from "../models/models_0";
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
 * The input for {@link ApplicableVariantsCommand}.
 */
export interface ApplicableVariantsCommandInput extends ApplicableVariantsInput {
}
/**
 * @public
 *
 * The output of {@link ApplicableVariantsCommand}.
 */
export interface ApplicableVariantsCommandOutput extends ApplicableVariantsOutput, __MetadataBearer {
}
declare const ApplicableVariantsCommand_base: {
    new (input: ApplicableVariantsCommandInput): import("@smithy/smithy-client").CommandImpl<ApplicableVariantsCommandInput, ApplicableVariantsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: ApplicableVariantsCommandInput): import("@smithy/smithy-client").CommandImpl<ApplicableVariantsCommandInput, ApplicableVariantsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ApplicableVariantsCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, ApplicableVariantsCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ApplicableVariantsInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   context: { // Condition // required
 *     "<keys>": "DOCUMENT_VALUE",
 *   },
 *   toss: Number("int"), // required
 * };
 * const command = new ApplicableVariantsCommand(input);
 * const response = await client.send(command);
 * // { // ApplicableVariantsOutput
 * //   data: [ // ListVariant // required
 * //     { // Variant
 * //       id: "STRING_VALUE", // required
 * //       variant_type: "CONTROL" || "EXPERIMENTAL", // required
 * //       context_id: "STRING_VALUE",
 * //       override_id: "STRING_VALUE",
 * //       overrides: "DOCUMENT_VALUE", // required
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param ApplicableVariantsCommandInput - {@link ApplicableVariantsCommandInput}
 * @returns {@link ApplicableVariantsCommandOutput}
 * @see {@link ApplicableVariantsCommandInput} for command's `input` shape.
 * @see {@link ApplicableVariantsCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class ApplicableVariantsCommand extends ApplicableVariantsCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: ApplicableVariantsInput;
            output: ApplicableVariantsOutput;
        };
        sdk: {
            input: ApplicableVariantsCommandInput;
            output: ApplicableVariantsCommandOutput;
        };
    };
}
