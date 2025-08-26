// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  WeightRecomputeInput,
  WeightRecomputeOutput,
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
export interface WeightRecomputeCommandOutput extends WeightRecomputeOutput, __MetadataBearer {}

/**
 * Recalculates and updates the priority weights for all contexts in the workspace based on their dimensions.
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
 * // { // WeightRecomputeOutput
 * //   data: [ // WeightRecomputeResponses
 * //     { // WeightRecomputeResponse
 * //       id: "STRING_VALUE",
 * //       condition: { // Condition
 * //         "<keys>": "DOCUMENT_VALUE",
 * //       },
 * //       old_weight: "STRING_VALUE",
 * //       new_weight: "STRING_VALUE",
 * //     },
 * //   ],
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
      output: WeightRecomputeOutput;
  };
  sdk: {
      input: WeightRecomputeCommandInput;
      output: WeightRecomputeCommandOutput;
  };
};
}
