// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ExperimentGroupResponse,
  ModifyMembersToGroupRequest,
} from "../models/models_0";
import {
  de_RemoveMembersFromGroupCommand,
  se_RemoveMembersFromGroupCommand,
} from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
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
export interface RemoveMembersFromGroupCommandInput extends ModifyMembersToGroupRequest {}
/**
 * @public
 *
 * The output of {@link RemoveMembersFromGroupCommand}.
 */
export interface RemoveMembersFromGroupCommandOutput extends ExperimentGroupResponse, __MetadataBearer {}

/**
 * Removes members from an existing experiment group.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, RemoveMembersFromGroupCommand } from "@juspay/superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, RemoveMembersFromGroupCommand } = require("@juspay/superposition-sdk"); // CommonJS import
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
 * //   buckets: [ // Buckets // required
 * //     { // Bucket
 * //       experiment_id: "STRING_VALUE", // required
 * //       variant_id: "STRING_VALUE", // required
 * //     },
 * //   ],
 * //   group_type: "USER_CREATED" || "SYSTEM_GENERATED", // required
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
export class RemoveMembersFromGroupCommand extends $Command.classBuilder<RemoveMembersFromGroupCommandInput, RemoveMembersFromGroupCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "RemoveMembersFromGroup", {

  })
  .n("SuperpositionClient", "RemoveMembersFromGroupCommand")
  .f(void 0, void 0)
  .ser(se_RemoveMembersFromGroupCommand)
  .de(de_RemoveMembersFromGroupCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
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
