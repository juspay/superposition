// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ExperimentResponse,
  PauseExperimentInput,
} from "../models/models_0";
import {
  de_PauseExperimentCommand,
  se_PauseExperimentCommand,
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
 * The input for {@link PauseExperimentCommand}.
 */
export interface PauseExperimentCommandInput extends PauseExperimentInput {}
/**
 * @public
 *
 * The output of {@link PauseExperimentCommand}.
 */
export interface PauseExperimentCommandOutput extends ExperimentResponse, __MetadataBearer {}

/**
 * Temporarily pauses an inprogress experiment, suspending its effects while preserving the experiment config for later resumption.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, PauseExperimentCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, PauseExperimentCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // PauseExperimentInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   id: "STRING_VALUE", // required
 *   change_reason: "STRING_VALUE", // required
 * };
 * const command = new PauseExperimentCommand(input);
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
 * @param PauseExperimentCommandInput - {@link PauseExperimentCommandInput}
 * @returns {@link PauseExperimentCommandOutput}
 * @see {@link PauseExperimentCommandInput} for command's `input` shape.
 * @see {@link PauseExperimentCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class PauseExperimentCommand extends $Command.classBuilder<PauseExperimentCommandInput, PauseExperimentCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "PauseExperiment", {

  })
  .n("SuperpositionClient", "PauseExperimentCommand")
  .f(void 0, void 0)
  .ser(se_PauseExperimentCommand)
  .de(de_PauseExperimentCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: PauseExperimentInput;
      output: ExperimentResponse;
  };
  sdk: {
      input: PauseExperimentCommandInput;
      output: PauseExperimentCommandOutput;
  };
};
}
