// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  DeleteExperimentGroupInput,
  ExperimentGroupResponse,
} from "../models/models_0";
import {
  de_DeleteExperimentGroupCommand,
  se_DeleteExperimentGroupCommand,
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
 * The input for {@link DeleteExperimentGroupCommand}.
 */
export interface DeleteExperimentGroupCommandInput extends DeleteExperimentGroupInput {}
/**
 * @public
 *
 * The output of {@link DeleteExperimentGroupCommand}.
 */
export interface DeleteExperimentGroupCommandOutput extends ExperimentGroupResponse, __MetadataBearer {}

/**
 * Deletes an experiment group.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, DeleteExperimentGroupCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, DeleteExperimentGroupCommand } = require("superposition-sdk"); // CommonJS import
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
export class DeleteExperimentGroupCommand extends $Command.classBuilder<DeleteExperimentGroupCommandInput, DeleteExperimentGroupCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "DeleteExperimentGroup", {

  })
  .n("SuperpositionClient", "DeleteExperimentGroupCommand")
  .f(void 0, void 0)
  .ser(se_DeleteExperimentGroupCommand)
  .de(de_DeleteExperimentGroupCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
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
