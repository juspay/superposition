// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ApplicableVariantsInput,
  ApplicableVariantsOutput,
} from "../models/models_0";
import {
  de_ApplicableVariantsCommand,
  se_ApplicableVariantsCommand,
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
 * The input for {@link ApplicableVariantsCommand}.
 */
export interface ApplicableVariantsCommandInput extends ApplicableVariantsInput {}
/**
 * @public
 *
 * The output of {@link ApplicableVariantsCommand}.
 */
export interface ApplicableVariantsCommandOutput extends ApplicableVariantsOutput, __MetadataBearer {}

/**
 * Determines which experiment variants are applicable to a given context, used for experiment evaluation and variant selection.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ApplicableVariantsCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ApplicableVariantsCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ApplicableVariantsInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   context: { // Condition // required
 *     "<keys>": "DOCUMENT_VALUE",
 *   },
 *   identifier: "STRING_VALUE", // required
 * };
 * const command = new ApplicableVariantsCommand(input);
 * const response = await client.send(command);
 * // { // ApplicableVariantsOutput
 * //   data: [ // ListVariant // required
 * //     { // Variant
 * //       id: "STRING_VALUE", // required
 * //       variant_type: "CONTROL" || "EXPERIMENTAL", // required
 * //       context_id: "STRING_VALUE",
 * //       override_id: "STRING_VALUE",
 * //       overrides: "DOCUMENT_VALUE", // required
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param ApplicableVariantsCommandInput - {@link ApplicableVariantsCommandInput}
 * @returns {@link ApplicableVariantsCommandOutput}
 * @see {@link ApplicableVariantsCommandInput} for command's `input` shape.
 * @see {@link ApplicableVariantsCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class ApplicableVariantsCommand extends $Command.classBuilder<ApplicableVariantsCommandInput, ApplicableVariantsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "ApplicableVariants", {

  })
  .n("SuperpositionClient", "ApplicableVariantsCommand")
  .f(void 0, void 0)
  .ser(se_ApplicableVariantsCommand)
  .de(de_ApplicableVariantsCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: ApplicableVariantsInput;
      output: ApplicableVariantsOutput;
  };
  sdk: {
      input: ApplicableVariantsCommandInput;
      output: ApplicableVariantsCommandOutput;
  };
};
}
