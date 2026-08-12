// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  JobCreateResponse,
  WeightRecomputeInput,
} from "../models/models_0";
import {
  de_WeightRecomputeCommand,
  se_WeightRecomputeCommand,
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
 * The input for {@link WeightRecomputeCommand}.
 */
export interface WeightRecomputeCommandInput extends WeightRecomputeInput {}
/**
 * @public
 *
 * The output of {@link WeightRecomputeCommand}.
 */
export interface WeightRecomputeCommandOutput extends JobCreateResponse, __MetadataBearer {}

/**
 * Recalculates and updates the priority weights for all contexts in the workspace based on their dimensions. This operation is asynchronous — it submits a background job and returns the job ID for polling.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, WeightRecomputeCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, WeightRecomputeCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // WeightRecomputeInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   config_tags: "STRING_VALUE",
 * };
 * const command = new WeightRecomputeCommand(input);
 * const response = await client.send(command);
 * // { // JobCreateResponse
 * //   id: "STRING_VALUE", // required
 * //   kronos_job_id: "STRING_VALUE", // required
 * //   status: "CREATED" || "SCHEDULED" || "INPROGRESS" || "FAILED" || "COMPLETED", // required
 * // };
 *
 * ```
 *
 * @param WeightRecomputeCommandInput - {@link WeightRecomputeCommandInput}
 * @returns {@link WeightRecomputeCommandOutput}
 * @see {@link WeightRecomputeCommandInput} for command's `input` shape.
 * @see {@link WeightRecomputeCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link WebhookFailed} (server fault)
 *  Indicates that the operation succeeded but the webhook call failed. The response body contains the successful result, but the client should be aware that webhook notification did not complete.
 *
 * @throws {@link WorkspaceLockConflict} (client fault)
 *  Returned when a workspace write operation cannot proceed because another write operation currently holds the workspace lock.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class WeightRecomputeCommand extends $Command.classBuilder<WeightRecomputeCommandInput, WeightRecomputeCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "WeightRecompute", {

  })
  .n("SuperpositionClient", "WeightRecomputeCommand")
  .f(void 0, void 0)
  .ser(se_WeightRecomputeCommand)
  .de(de_WeightRecomputeCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: WeightRecomputeInput;
      output: JobCreateResponse;
  };
  sdk: {
      input: WeightRecomputeCommandInput;
      output: WeightRecomputeCommandOutput;
  };
};
}
