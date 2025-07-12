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
  de_AddMembersToGroupCommand,
  se_AddMembersToGroupCommand,
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
 * The input for {@link AddMembersToGroupCommand}.
 */
export interface AddMembersToGroupCommandInput extends ModifyMembersToGroupRequest {}
/**
 * @public
 *
 * The output of {@link AddMembersToGroupCommand}.
 */
export interface AddMembersToGroupCommandOutput extends ExperimentGroupResponse, __MetadataBearer {}

/**
 * Adds members to an existing experiment group.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, AddMembersToGroupCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, AddMembersToGroupCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
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
 * const command = new AddMembersToGroupCommand(input);
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
 * @param AddMembersToGroupCommandInput - {@link AddMembersToGroupCommandInput}
 * @returns {@link AddMembersToGroupCommandOutput}
 * @see {@link AddMembersToGroupCommandInput} for command's `input` shape.
 * @see {@link AddMembersToGroupCommandOutput} for command's `response` shape.
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
export class AddMembersToGroupCommand extends $Command.classBuilder<AddMembersToGroupCommandInput, AddMembersToGroupCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "AddMembersToGroup", {

  })
  .n("SuperpositionClient", "AddMembersToGroupCommand")
  .f(void 0, void 0)
  .ser(se_AddMembersToGroupCommand)
  .de(de_AddMembersToGroupCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: ModifyMembersToGroupRequest;
      output: ExperimentGroupResponse;
  };
  sdk: {
      input: AddMembersToGroupCommandInput;
      output: AddMembersToGroupCommandOutput;
  };
};
}
