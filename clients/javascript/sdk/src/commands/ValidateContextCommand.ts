// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import { ValidateContextInput } from "../models/models_0";
import {
  de_ValidateContextCommand,
  se_ValidateContextCommand,
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
 * The input for {@link ValidateContextCommand}.
 */
export interface ValidateContextCommandInput extends ValidateContextInput {}
/**
 * @public
 *
 * The output of {@link ValidateContextCommand}.
 */
export interface ValidateContextCommandOutput extends __MetadataBearer {}

/**
 * Validates if a given context condition is well-formed
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ValidateContextCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ValidateContextCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ValidateContextInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   context: { // Condition // required
 *     "<keys>": "DOCUMENT_VALUE",
 *   },
 * };
 * const command = new ValidateContextCommand(input);
 * const response = await client.send(command);
 * // {};
 *
 * ```
 *
 * @param ValidateContextCommandInput - {@link ValidateContextCommandInput}
 * @returns {@link ValidateContextCommandOutput}
 * @see {@link ValidateContextCommandInput} for command's `input` shape.
 * @see {@link ValidateContextCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class ValidateContextCommand extends $Command.classBuilder<ValidateContextCommandInput, ValidateContextCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "ValidateContext", {

  })
  .n("SuperpositionClient", "ValidateContextCommand")
  .f(void 0, void 0)
  .ser(se_ValidateContextCommand)
  .de(de_ValidateContextCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: ValidateContextInput;
      output: {};
  };
  sdk: {
      input: ValidateContextCommandInput;
      output: ValidateContextCommandOutput;
  };
};
}
