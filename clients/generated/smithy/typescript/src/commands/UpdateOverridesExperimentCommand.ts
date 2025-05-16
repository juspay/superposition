// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ExperimentResponse,
  UpdateOverrideRequest,
} from "../models/models_0";
import {
  de_UpdateOverridesExperimentCommand,
  se_UpdateOverridesExperimentCommand,
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
 * The input for {@link UpdateOverridesExperimentCommand}.
 */
export interface UpdateOverridesExperimentCommandInput extends UpdateOverrideRequest {}
/**
 * @public
 *
 * The output of {@link UpdateOverridesExperimentCommand}.
 */
export interface UpdateOverridesExperimentCommandOutput extends ExperimentResponse, __MetadataBearer {}

/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, UpdateOverridesExperimentCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, UpdateOverridesExperimentCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // UpdateOverrideRequest
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   id: "STRING_VALUE", // required
 *   variant_list: [ // ListVariantUpdateRequest // required
 *     { // VariantUpdateRequest
 *       id: "STRING_VALUE", // required
 *       overrides: "DOCUMENT_VALUE", // required
 *     },
 *   ],
 *   description: "STRING_VALUE",
 *   change_reason: "STRING_VALUE", // required
 * };
 * const command = new UpdateOverridesExperimentCommand(input);
 * const response = await client.send(command);
 * // { // ExperimentResponse
 * //   id: "STRING_VALUE", // required
 * //   created_at: new Date("TIMESTAMP"), // required
 * //   created_by: "STRING_VALUE", // required
 * //   last_modified: new Date("TIMESTAMP"), // required
 * //   name: "STRING_VALUE", // required
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
 * // };
 *
 * ```
 *
 * @param UpdateOverridesExperimentCommandInput - {@link UpdateOverridesExperimentCommandInput}
 * @returns {@link UpdateOverridesExperimentCommandOutput}
 * @see {@link UpdateOverridesExperimentCommandInput} for command's `input` shape.
 * @see {@link UpdateOverridesExperimentCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export class UpdateOverridesExperimentCommand extends $Command.classBuilder<UpdateOverridesExperimentCommandInput, UpdateOverridesExperimentCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "UpdateOverridesExperiment", {

  })
  .n("SuperpositionClient", "UpdateOverridesExperimentCommand")
  .f(void 0, void 0)
  .ser(se_UpdateOverridesExperimentCommand)
  .de(de_UpdateOverridesExperimentCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: UpdateOverrideRequest;
      output: ExperimentResponse;
  };
  sdk: {
      input: UpdateOverridesExperimentCommandInput;
      output: UpdateOverridesExperimentCommandOutput;
  };
};
}
