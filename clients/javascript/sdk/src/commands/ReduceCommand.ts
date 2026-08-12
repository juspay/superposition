// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  JobCreateResponse,
  ReduceInput,
} from "../models/models_0";
import {
  de_ReduceCommand,
  se_ReduceCommand,
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
 * The input for {@link ReduceCommand}.
 */
export interface ReduceCommandInput extends ReduceInput {}
/**
 * @public
 *
 * The output of {@link ReduceCommand}.
 */
export interface ReduceCommandOutput extends JobCreateResponse, __MetadataBearer {}

/**
 * Reduces the configuration by removing redundant overrides across contexts. This operation is asynchronous — it submits a background job and returns the job ID for polling.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ReduceCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ReduceCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ReduceInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 * };
 * const command = new ReduceCommand(input);
 * const response = await client.send(command);
 * // { // JobCreateResponse
 * //   id: "STRING_VALUE", // required
 * //   kronos_job_id: "STRING_VALUE", // required
 * //   status: "CREATED" || "SCHEDULED" || "INPROGRESS" || "FAILED" || "COMPLETED", // required
 * // };
 *
 * ```
 *
 * @param ReduceCommandInput - {@link ReduceCommandInput}
 * @returns {@link ReduceCommandOutput}
 * @see {@link ReduceCommandInput} for command's `input` shape.
 * @see {@link ReduceCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class ReduceCommand extends $Command.classBuilder<ReduceCommandInput, ReduceCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "Reduce", {

  })
  .n("SuperpositionClient", "ReduceCommand")
  .f(void 0, void 0)
  .ser(se_ReduceCommand)
  .de(de_ReduceCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: ReduceInput;
      output: JobCreateResponse;
  };
  sdk: {
      input: ReduceCommandInput;
      output: ReduceCommandOutput;
  };
};
}
