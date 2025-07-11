import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { DeleteExperimentGroupInput, ExperimentGroupResponse } from "../models/models_0";
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
 * The input for {@link DeleteExperimentGroupCommand}.
 */
export interface DeleteExperimentGroupCommandInput extends DeleteExperimentGroupInput {
}
/**
 * @public
 *
 * The output of {@link DeleteExperimentGroupCommand}.
 */
export interface DeleteExperimentGroupCommandOutput extends ExperimentGroupResponse, __MetadataBearer {
}
declare const DeleteExperimentGroupCommand_base: {
    new (input: DeleteExperimentGroupCommandInput): import("@smithy/smithy-client").CommandImpl<DeleteExperimentGroupCommandInput, DeleteExperimentGroupCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: DeleteExperimentGroupCommandInput): import("@smithy/smithy-client").CommandImpl<DeleteExperimentGroupCommandInput, DeleteExperimentGroupCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * Deletes an experiment group.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, DeleteExperimentGroupCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, DeleteExperimentGroupCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // DeleteExperimentGroupInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   id: "STRING_VALUE", // required
 * };
 * const command = new DeleteExperimentGroupCommand(input);
 * const response = await client.send(command);
 * // { // ExperimentGroupResponse
 * //   id: "STRING_VALUE", // required
 * //   context_hash: "STRING_VALUE", // required
 * //   name: "STRING_VALUE", // required
 * //   description: "STRING_VALUE", // required
 * //   change_reason: "STRING_VALUE", // required
 * //   context: { // Condition // required
 * //     "<keys>": "DOCUMENT_VALUE",
 * //   },
 * //   traffic_percentage: Number("int"), // required
 * //   member_experiment_ids: [ // StringList // required
 * //     "STRING_VALUE",
 * //   ],
 * //   created_at: new Date("TIMESTAMP"), // required
 * //   created_by: "STRING_VALUE", // required
 * //   last_modified_at: new Date("TIMESTAMP"), // required
 * //   last_modified_by: "STRING_VALUE", // required
 * // };
 *
 * ```
 *
 * @param DeleteExperimentGroupCommandInput - {@link DeleteExperimentGroupCommandInput}
 * @returns {@link DeleteExperimentGroupCommandOutput}
 * @see {@link DeleteExperimentGroupCommandInput} for command's `input` shape.
 * @see {@link DeleteExperimentGroupCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link ResourceNotFound} (client fault)
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export declare class DeleteExperimentGroupCommand extends DeleteExperimentGroupCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: DeleteExperimentGroupInput;
            output: ExperimentGroupResponse;
        };
        sdk: {
            input: DeleteExperimentGroupCommandInput;
            output: DeleteExperimentGroupCommandOutput;
        };
    };
}
