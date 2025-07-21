// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ExperimentGroupResponse,
  UpdateExperimentGroupRequest,
} from "../models/models_0";
import {
  de_UpdateExperimentGroupCommand,
  se_UpdateExperimentGroupCommand,
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
 * The input for {@link UpdateExperimentGroupCommand}.
 */
export interface UpdateExperimentGroupCommandInput extends UpdateExperimentGroupRequest {}
/**
 * @public
 *
 * The output of {@link UpdateExperimentGroupCommand}.
 */
export interface UpdateExperimentGroupCommandOutput extends ExperimentGroupResponse, __MetadataBearer {}

/**
 * Updates an existing experiment group. Allows partial updates to specified fields.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, UpdateExperimentGroupCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, UpdateExperimentGroupCommand } = require("superposition-sdk"); // CommonJS import
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
export class UpdateExperimentGroupCommand extends $Command.classBuilder<UpdateExperimentGroupCommandInput, UpdateExperimentGroupCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "UpdateExperimentGroup", {

  })
  .n("SuperpositionClient", "UpdateExperimentGroupCommand")
  .f(void 0, void 0)
  .ser(se_UpdateExperimentGroupCommand)
  .de(de_UpdateExperimentGroupCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
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
