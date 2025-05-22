import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { ConcludeExperimentInput, ExperimentResponse } from "../models/models_0";
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
 * The input for {@link ConcludeExperimentCommand}.
 */
export interface ConcludeExperimentCommandInput extends ConcludeExperimentInput {
}
/**
 * @public
 *
 * The output of {@link ConcludeExperimentCommand}.
 */
export interface ConcludeExperimentCommandOutput extends ExperimentResponse, __MetadataBearer {
}
declare const ConcludeExperimentCommand_base: {
    new (input: ConcludeExperimentCommandInput): import("@smithy/smithy-client").CommandImpl<ConcludeExperimentCommandInput, ConcludeExperimentCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: ConcludeExperimentCommandInput): import("@smithy/smithy-client").CommandImpl<ConcludeExperimentCommandInput, ConcludeExperimentCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ConcludeExperimentCommand } from "@io.juspay/superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ConcludeExperimentCommand } = require("@io.juspay/superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ConcludeExperimentInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   id: "STRING_VALUE", // required
 *   chosen_variant: "STRING_VALUE", // required
 *   description: "STRING_VALUE",
 *   change_reason: "STRING_VALUE", // required
 * };
 * const command = new ConcludeExperimentCommand(input);
 * const response = await client.send(command);
 * // { // ExperimentResponse
 * //   id: "STRING_VALUE", // required
 * //   created_at: new Date("TIMESTAMP"), // required
 * //   created_by: "STRING_VALUE", // required
 * //   last_modified: new Date("TIMESTAMP"), // required
 * //   name: "STRING_VALUE", // required
 * //   override_keys: [ // ListOverrideKeys // required
 * //     "STRING_VALUE",
 * //   ],
 * //   status: "CREATED" || "CONCLUDED" || "INPROGRESS" || "DISCARDED", // required
 * //   traffic_percentage: Number("int"), // required
 * //   context: { // Condition // required
 * //     "<keys>": "DOCUMENT_VALUE",
 * //   },
 * //   variants: [ // ListVariant // required
 * //     { // Variant
 * //       id: "STRING_VALUE", // required
 * //       variant_type: "CONTROL" || "EXPERIMENTAL", // required
 * //       context_id: "STRING_VALUE",
 * //       override_id: "STRING_VALUE",
 * //       overrides: "DOCUMENT_VALUE", // required
 * //     },
 * //   ],
 * //   last_modified_by: "STRING_VALUE", // required
 * //   chosen_variant: "STRING_VALUE",
 * //   description: "STRING_VALUE", // required
 * //   change_reason: "STRING_VALUE", // required
 * // };
 *
 * ```
 *
 * @param ConcludeExperimentCommandInput - {@link ConcludeExperimentCommandInput}
 * @returns {@link ConcludeExperimentCommandOutput}
 * @see {@link ConcludeExperimentCommandInput} for command's `input` shape.
 * @see {@link ConcludeExperimentCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class ConcludeExperimentCommand extends ConcludeExperimentCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: ConcludeExperimentInput;
            output: ExperimentResponse;
        };
        sdk: {
            input: ConcludeExperimentCommandInput;
            output: ConcludeExperimentCommandOutput;
        };
    };
}
