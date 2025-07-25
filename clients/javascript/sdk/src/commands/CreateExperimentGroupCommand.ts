// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  CreateExperimentGroupRequest,
  ExperimentGroupResponse,
} from "../models/models_0";
import {
  de_CreateExperimentGroupCommand,
  se_CreateExperimentGroupCommand,
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
 * The input for {@link CreateExperimentGroupCommand}.
 */
export interface CreateExperimentGroupCommandInput extends CreateExperimentGroupRequest {}
/**
 * @public
 *
 * The output of {@link CreateExperimentGroupCommand}.
 */
export interface CreateExperimentGroupCommandOutput extends ExperimentGroupResponse, __MetadataBearer {}

/**
 * Creates a new experiment group.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, CreateExperimentGroupCommand } from "@juspay/superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, CreateExperimentGroupCommand } = require("@juspay/superposition-sdk"); // CommonJS import
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
export class CreateExperimentGroupCommand extends $Command.classBuilder<CreateExperimentGroupCommandInput, CreateExperimentGroupCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "CreateExperimentGroup", {

  })
  .n("SuperpositionClient", "CreateExperimentGroupCommand")
  .f(void 0, void 0)
  .ser(se_CreateExperimentGroupCommand)
  .de(de_CreateExperimentGroupCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
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
