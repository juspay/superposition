import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { ExperimentGroupResponse, UpdateExperimentGroupRequest } from "../models/models_0";
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
 * The input for {@link UpdateExperimentGroupCommand}.
 */
export interface UpdateExperimentGroupCommandInput extends UpdateExperimentGroupRequest {
}
/**
 * @public
 *
 * The output of {@link UpdateExperimentGroupCommand}.
 */
export interface UpdateExperimentGroupCommandOutput extends ExperimentGroupResponse, __MetadataBearer {
}
declare const UpdateExperimentGroupCommand_base: {
    new (input: UpdateExperimentGroupCommandInput): import("@smithy/smithy-client").CommandImpl<UpdateExperimentGroupCommandInput, UpdateExperimentGroupCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: UpdateExperimentGroupCommandInput): import("@smithy/smithy-client").CommandImpl<UpdateExperimentGroupCommandInput, UpdateExperimentGroupCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * Updates an existing experiment group. Allows partial updates to specified fields.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, UpdateExperimentGroupCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, UpdateExperimentGroupCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // UpdateExperimentGroupRequest
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   id: "STRING_VALUE", // required
 *   change_reason: "STRING_VALUE", // required
 *   description: "STRING_VALUE",
 *   traffic_percentage: Number("int"),
 * };
 * const command = new UpdateExperimentGroupCommand(input);
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
 * @param UpdateExperimentGroupCommandInput - {@link UpdateExperimentGroupCommandInput}
 * @returns {@link UpdateExperimentGroupCommandOutput}
 * @see {@link UpdateExperimentGroupCommandInput} for command's `input` shape.
 * @see {@link UpdateExperimentGroupCommandOutput} for command's `response` shape.
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
export declare class UpdateExperimentGroupCommand extends UpdateExperimentGroupCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: UpdateExperimentGroupRequest;
            output: ExperimentGroupResponse;
        };
        sdk: {
            input: UpdateExperimentGroupCommandInput;
            output: UpdateExperimentGroupCommandOutput;
        };
    };
}
