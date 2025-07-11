import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { CreateExperimentRequest, ExperimentResponse } from "../models/models_0";
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
 * The input for {@link CreateExperimentCommand}.
 */
export interface CreateExperimentCommandInput extends CreateExperimentRequest {
}
/**
 * @public
 *
 * The output of {@link CreateExperimentCommand}.
 */
export interface CreateExperimentCommandOutput extends ExperimentResponse, __MetadataBearer {
}
declare const CreateExperimentCommand_base: {
    new (input: CreateExperimentCommandInput): import("@smithy/smithy-client").CommandImpl<CreateExperimentCommandInput, CreateExperimentCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: CreateExperimentCommandInput): import("@smithy/smithy-client").CommandImpl<CreateExperimentCommandInput, CreateExperimentCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, CreateExperimentCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, CreateExperimentCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // CreateExperimentRequest
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   name: "STRING_VALUE", // required
 *   experiment_type: "DEFAULT" || "DELETE_OVERRIDES",
 *   context: { // Condition // required
 *     "<keys>": "DOCUMENT_VALUE",
 *   },
 *   variants: [ // ListVariant // required
 *     { // Variant
 *       id: "STRING_VALUE", // required
 *       variant_type: "CONTROL" || "EXPERIMENTAL", // required
 *       context_id: "STRING_VALUE",
 *       override_id: "STRING_VALUE",
 *       overrides: "DOCUMENT_VALUE", // required
 *     },
 *   ],
 *   description: "STRING_VALUE", // required
 *   change_reason: "STRING_VALUE", // required
 *   metrics: "DOCUMENT_VALUE",
 *   experiment_group_id: "STRING_VALUE",
 * };
 * const command = new CreateExperimentCommand(input);
 * const response = await client.send(command);
 * // { // ExperimentResponse
 * //   id: "STRING_VALUE", // required
 * //   created_at: new Date("TIMESTAMP"), // required
 * //   created_by: "STRING_VALUE", // required
 * //   last_modified: new Date("TIMESTAMP"), // required
 * //   name: "STRING_VALUE", // required
 * //   experiment_type: "DEFAULT" || "DELETE_OVERRIDES", // required
 * //   override_keys: [ // ListOverrideKeys // required
 * //     "STRING_VALUE",
 * //   ],
 * //   status: "CREATED" || "CONCLUDED" || "INPROGRESS" || "DISCARDED" || "PAUSED", // required
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
 * //   started_at: new Date("TIMESTAMP"),
 * //   started_by: "STRING_VALUE",
 * //   metrics_url: "STRING_VALUE",
 * //   metrics: "DOCUMENT_VALUE",
 * //   experiment_group_id: "STRING_VALUE",
 * // };
 *
 * ```
 *
 * @param CreateExperimentCommandInput - {@link CreateExperimentCommandInput}
 * @returns {@link CreateExperimentCommandOutput}
 * @see {@link CreateExperimentCommandInput} for command's `input` shape.
 * @see {@link CreateExperimentCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class CreateExperimentCommand extends CreateExperimentCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: CreateExperimentRequest;
            output: ExperimentResponse;
        };
        sdk: {
            input: CreateExperimentCommandInput;
            output: CreateExperimentCommandOutput;
        };
    };
}
