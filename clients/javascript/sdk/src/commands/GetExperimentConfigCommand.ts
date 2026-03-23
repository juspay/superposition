// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  GetExperimentConfigInput,
  GetExperimentConfigOutput,
} from "../models/models_0";
import {
  de_GetExperimentConfigCommand,
  se_GetExperimentConfigCommand,
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
 * The input for {@link GetExperimentConfigCommand}.
 */
export interface GetExperimentConfigCommandInput extends GetExperimentConfigInput {}
/**
 * @public
 *
 * The output of {@link GetExperimentConfigCommand}.
 */
export interface GetExperimentConfigCommandOutput extends GetExperimentConfigOutput, __MetadataBearer {}

/**
 * Retrieves the experiment configuration for a given workspace and organization. The response includes details of all experiment groups and experiments that match the specified filters.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetExperimentConfigCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetExperimentConfigCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetExperimentConfigInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   if_modified_since: new Date("TIMESTAMP"),
 *   prefix: [ // StringList
 *     "STRING_VALUE",
 *   ],
 *   context: { // ContextMap
 *     "<keys>": "DOCUMENT_VALUE",
 *   },
 *   dimension_match_strategy: "exact" || "subset",
 * };
 * const command = new GetExperimentConfigCommand(input);
 * const response = await client.send(command);
 * // { // GetExperimentConfigOutput
 * //   last_modified: new Date("TIMESTAMP"), // required
 * //   experiments: [ // ExperimentList // required
 * //     { // ExperimentResponse
 * //       id: "STRING_VALUE", // required
 * //       created_at: new Date("TIMESTAMP"), // required
 * //       created_by: "STRING_VALUE", // required
 * //       last_modified: new Date("TIMESTAMP"), // required
 * //       name: "STRING_VALUE", // required
 * //       experiment_type: "DEFAULT" || "DELETE_OVERRIDES", // required
 * //       override_keys: [ // ListOverrideKeys // required
 * //         "STRING_VALUE",
 * //       ],
 * //       status: "CREATED" || "CONCLUDED" || "INPROGRESS" || "DISCARDED" || "PAUSED", // required
 * //       traffic_percentage: Number("int"), // required
 * //       context: { // Condition // required
 * //         "<keys>": "DOCUMENT_VALUE",
 * //       },
 * //       variants: [ // ListVariant // required
 * //         { // Variant
 * //           id: "STRING_VALUE", // required
 * //           variant_type: "CONTROL" || "EXPERIMENTAL", // required
 * //           context_id: "STRING_VALUE",
 * //           override_id: "STRING_VALUE",
 * //           overrides: { // Overrides // required
 * //             "<keys>": "DOCUMENT_VALUE",
 * //           },
 * //         },
 * //       ],
 * //       last_modified_by: "STRING_VALUE", // required
 * //       chosen_variant: "STRING_VALUE",
 * //       description: "STRING_VALUE", // required
 * //       change_reason: "STRING_VALUE", // required
 * //       started_at: new Date("TIMESTAMP"),
 * //       started_by: "STRING_VALUE",
 * //       metrics_url: "STRING_VALUE",
 * //       metrics: "DOCUMENT_VALUE",
 * //       experiment_group_id: "STRING_VALUE",
 * //     },
 * //   ],
 * //   experiment_groups: [ // ExperimentGroupList // required
 * //     { // ExperimentGroupResponse
 * //       id: "STRING_VALUE", // required
 * //       context_hash: "STRING_VALUE", // required
 * //       name: "STRING_VALUE", // required
 * //       description: "STRING_VALUE", // required
 * //       change_reason: "STRING_VALUE", // required
 * //       context: { // required
 * //         "<keys>": "DOCUMENT_VALUE",
 * //       },
 * //       traffic_percentage: Number("int"), // required
 * //       member_experiment_ids: [ // StringList // required
 * //         "STRING_VALUE",
 * //       ],
 * //       created_at: new Date("TIMESTAMP"), // required
 * //       created_by: "STRING_VALUE", // required
 * //       last_modified_at: new Date("TIMESTAMP"), // required
 * //       last_modified_by: "STRING_VALUE", // required
 * //       buckets: [ // Buckets // required
 * //         { // Bucket
 * //           experiment_id: "STRING_VALUE", // required
 * //           variant_id: "STRING_VALUE", // required
 * //         },
 * //       ],
 * //       group_type: "USER_CREATED" || "SYSTEM_GENERATED", // required
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param GetExperimentConfigCommandInput - {@link GetExperimentConfigCommandInput}
 * @returns {@link GetExperimentConfigCommandOutput}
 * @see {@link GetExperimentConfigCommandInput} for command's `input` shape.
 * @see {@link GetExperimentConfigCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class GetExperimentConfigCommand extends $Command.classBuilder<GetExperimentConfigCommandInput, GetExperimentConfigCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetExperimentConfig", {

  })
  .n("SuperpositionClient", "GetExperimentConfigCommand")
  .f(void 0, void 0)
  .ser(se_GetExperimentConfigCommand)
  .de(de_GetExperimentConfigCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetExperimentConfigInput;
      output: GetExperimentConfigOutput;
  };
  sdk: {
      input: GetExperimentConfigCommandInput;
      output: GetExperimentConfigCommandOutput;
  };
};
}
