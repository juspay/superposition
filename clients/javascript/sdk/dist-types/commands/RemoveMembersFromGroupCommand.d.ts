import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { ExperimentGroupResponse, ModifyMembersToGroupRequest } from "../models/models_0";
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
 * The input for {@link RemoveMembersFromGroupCommand}.
 */
export interface RemoveMembersFromGroupCommandInput extends ModifyMembersToGroupRequest {
}
/**
 * @public
 *
 * The output of {@link RemoveMembersFromGroupCommand}.
 */
export interface RemoveMembersFromGroupCommandOutput extends ExperimentGroupResponse, __MetadataBearer {
}
declare const RemoveMembersFromGroupCommand_base: {
    new (input: RemoveMembersFromGroupCommandInput): import("@smithy/smithy-client").CommandImpl<RemoveMembersFromGroupCommandInput, RemoveMembersFromGroupCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: RemoveMembersFromGroupCommandInput): import("@smithy/smithy-client").CommandImpl<RemoveMembersFromGroupCommandInput, RemoveMembersFromGroupCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * Removes members from an existing experiment group.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, RemoveMembersFromGroupCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, RemoveMembersFromGroupCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ModifyMembersToGroupRequest
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   id: "STRING_VALUE", // required
 *   change_reason: "STRING_VALUE", // required
 *   member_experiment_ids: [ // StringList // required
 *     "STRING_VALUE",
 *   ],
 * };
 * const command = new RemoveMembersFromGroupCommand(input);
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
 * @param RemoveMembersFromGroupCommandInput - {@link RemoveMembersFromGroupCommandInput}
 * @returns {@link RemoveMembersFromGroupCommandOutput}
 * @see {@link RemoveMembersFromGroupCommandInput} for command's `input` shape.
 * @see {@link RemoveMembersFromGroupCommandOutput} for command's `response` shape.
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
export declare class RemoveMembersFromGroupCommand extends RemoveMembersFromGroupCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: ModifyMembersToGroupRequest;
            output: ExperimentGroupResponse;
        };
        sdk: {
            input: RemoveMembersFromGroupCommandInput;
            output: RemoveMembersFromGroupCommandOutput;
        };
    };
}
