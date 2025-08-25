// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ExperimentResponse,
  ResumeExperimentInput,
} from "../models/models_0";
import {
  de_ResumeExperimentCommand,
  se_ResumeExperimentCommand,
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
 * The input for {@link ResumeExperimentCommand}.
 */
export interface ResumeExperimentCommandInput extends ResumeExperimentInput {}
/**
 * @public
 *
 * The output of {@link ResumeExperimentCommand}.
 */
export interface ResumeExperimentCommandOutput extends ExperimentResponse, __MetadataBearer {}

/**
 * Resumes a previously paused experiment, restoring its in-progress state and re-enabling variant evaluation.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ResumeExperimentCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ResumeExperimentCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ResumeExperimentInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   id: "STRING_VALUE", // required
 *   change_reason: "STRING_VALUE", // required
 * };
 * const command = new ResumeExperimentCommand(input);
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
 * @param ResumeExperimentCommandInput - {@link ResumeExperimentCommandInput}
 * @returns {@link ResumeExperimentCommandOutput}
 * @see {@link ResumeExperimentCommandInput} for command's `input` shape.
 * @see {@link ResumeExperimentCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class ResumeExperimentCommand extends $Command.classBuilder<ResumeExperimentCommandInput, ResumeExperimentCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "ResumeExperiment", {

  })
  .n("SuperpositionClient", "ResumeExperimentCommand")
  .f(void 0, void 0)
  .ser(se_ResumeExperimentCommand)
  .de(de_ResumeExperimentCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: ResumeExperimentInput;
      output: ExperimentResponse;
  };
  sdk: {
      input: ResumeExperimentCommandInput;
      output: ResumeExperimentCommandOutput;
  };
};
}
