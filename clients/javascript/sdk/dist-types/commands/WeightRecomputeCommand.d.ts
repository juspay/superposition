import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { WeightRecomputeInput, WeightRecomputeOutput } from "../models/models_0";
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
 * The input for {@link WeightRecomputeCommand}.
 */
export interface WeightRecomputeCommandInput extends WeightRecomputeInput {
}
/**
 * @public
 *
 * The output of {@link WeightRecomputeCommand}.
 */
export interface WeightRecomputeCommandOutput extends WeightRecomputeOutput, __MetadataBearer {
}
declare const WeightRecomputeCommand_base: {
    new (input: WeightRecomputeCommandInput): import("@smithy/smithy-client").CommandImpl<WeightRecomputeCommandInput, WeightRecomputeCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: WeightRecomputeCommandInput): import("@smithy/smithy-client").CommandImpl<WeightRecomputeCommandInput, WeightRecomputeCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, WeightRecomputeCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, WeightRecomputeCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // WeightRecomputeInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   config_tags: "STRING_VALUE",
 * };
 * const command = new WeightRecomputeCommand(input);
 * const response = await client.send(command);
 * // { // WeightRecomputeOutput
 * //   data: [ // WeightRecomputeResponses
 * //     { // WeightRecomputeResponse
 * //       id: "STRING_VALUE",
 * //       condition: { // Condition
 * //         "<keys>": "DOCUMENT_VALUE",
 * //       },
 * //       old_weight: "STRING_VALUE",
 * //       new_weight: "STRING_VALUE",
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param WeightRecomputeCommandInput - {@link WeightRecomputeCommandInput}
 * @returns {@link WeightRecomputeCommandOutput}
 * @see {@link WeightRecomputeCommandInput} for command's `input` shape.
 * @see {@link WeightRecomputeCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class WeightRecomputeCommand extends WeightRecomputeCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: WeightRecomputeInput;
            output: WeightRecomputeOutput;
        };
        sdk: {
            input: WeightRecomputeCommandInput;
            output: WeightRecomputeCommandOutput;
        };
    };
}
