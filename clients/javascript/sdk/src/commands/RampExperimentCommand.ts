// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ExperimentResponse,
  RampExperimentInput,
} from "../models/models_0";
import {
  de_RampExperimentCommand,
  se_RampExperimentCommand,
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
 * The input for {@link RampExperimentCommand}.
 */
export interface RampExperimentCommandInput extends RampExperimentInput {}
/**
 * @public
 *
 * The output of {@link RampExperimentCommand}.
 */
export interface RampExperimentCommandOutput extends ExperimentResponse, __MetadataBearer {}

/**
 * Adjusts the traffic percentage allocation for an in-progress experiment, allowing gradual rollout or rollback of experimental features.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, RampExperimentCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, RampExperimentCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // RampExperimentInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   id: "STRING_VALUE", // required
 *   change_reason: "STRING_VALUE", // required
 *   traffic_percentage: Number("int"), // required
 * };
 * const command = new RampExperimentCommand(input);
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
 * @param RampExperimentCommandInput - {@link RampExperimentCommandInput}
 * @returns {@link RampExperimentCommandOutput}
 * @see {@link RampExperimentCommandInput} for command's `input` shape.
 * @see {@link RampExperimentCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class RampExperimentCommand extends $Command.classBuilder<RampExperimentCommandInput, RampExperimentCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "RampExperiment", {

  })
  .n("SuperpositionClient", "RampExperimentCommand")
  .f(void 0, void 0)
  .ser(se_RampExperimentCommand)
  .de(de_RampExperimentCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: RampExperimentInput;
      output: ExperimentResponse;
  };
  sdk: {
      input: RampExperimentCommandInput;
      output: RampExperimentCommandOutput;
  };
};
}
