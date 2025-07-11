import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { CreateExperimentGroupRequest, ExperimentGroupResponse } from "../models/models_0";
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
 * The input for {@link CreateExperimentGroupCommand}.
 */
export interface CreateExperimentGroupCommandInput extends CreateExperimentGroupRequest {
}
/**
 * @public
 *
 * The output of {@link CreateExperimentGroupCommand}.
 */
export interface CreateExperimentGroupCommandOutput extends ExperimentGroupResponse, __MetadataBearer {
}
declare const CreateExperimentGroupCommand_base: {
    new (input: CreateExperimentGroupCommandInput): import("@smithy/smithy-client").CommandImpl<CreateExperimentGroupCommandInput, CreateExperimentGroupCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: CreateExperimentGroupCommandInput): import("@smithy/smithy-client").CommandImpl<CreateExperimentGroupCommandInput, CreateExperimentGroupCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * Creates a new experiment group.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, CreateExperimentGroupCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, CreateExperimentGroupCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // CreateExperimentGroupRequest
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   name: "STRING_VALUE", // required
 *   description: "STRING_VALUE", // required
 *   change_reason: "STRING_VALUE", // required
 *   context: { // Condition // required
 *     "<keys>": "DOCUMENT_VALUE",
 *   },
 *   traffic_percentage: Number("int"), // required
 *   member_experiment_ids: [ // StringList
 *     "STRING_VALUE",
 *   ],
 * };
 * const command = new CreateExperimentGroupCommand(input);
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
 * @param CreateExperimentGroupCommandInput - {@link CreateExperimentGroupCommandInput}
 * @returns {@link CreateExperimentGroupCommandOutput}
 * @see {@link CreateExperimentGroupCommandInput} for command's `input` shape.
 * @see {@link CreateExperimentGroupCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export declare class CreateExperimentGroupCommand extends CreateExperimentGroupCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: CreateExperimentGroupRequest;
            output: ExperimentGroupResponse;
        };
        sdk: {
            input: CreateExperimentGroupCommandInput;
            output: CreateExperimentGroupCommandOutput;
        };
    };
}
