// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ContextActionResponse,
  MoveContextInput,
} from "../models/models_0";
import {
  de_MoveContextCommand,
  se_MoveContextCommand,
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
 * The input for {@link MoveContextCommand}.
 */
export interface MoveContextCommandInput extends MoveContextInput {}
/**
 * @public
 *
 * The output of {@link MoveContextCommand}.
 */
export interface MoveContextCommandOutput extends ContextActionResponse, __MetadataBearer {}

/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, MoveContextCommand } from "@io.juspay/superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, MoveContextCommand } = require("@io.juspay/superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // MoveContextInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   id: "STRING_VALUE", // required
 *   context: { // Condition // required
 *     "<keys>": "DOCUMENT_VALUE",
 *   },
 *   description: "STRING_VALUE",
 *   change_reason: "STRING_VALUE", // required
 * };
 * const command = new MoveContextCommand(input);
 * const response = await client.send(command);
 * // { // ContextActionResponse
 * //   context_id: "STRING_VALUE", // required
 * //   override_id: "STRING_VALUE", // required
 * //   weight: "STRING_VALUE", // required
 * //   description: "STRING_VALUE", // required
 * //   change_reason: "STRING_VALUE", // required
 * // };
 *
 * ```
 *
 * @param MoveContextCommandInput - {@link MoveContextCommandInput}
 * @returns {@link MoveContextCommandOutput}
 * @see {@link MoveContextCommandInput} for command's `input` shape.
 * @see {@link MoveContextCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link ResourceNotFound} (client fault)
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export class MoveContextCommand extends $Command.classBuilder<MoveContextCommandInput, MoveContextCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "MoveContext", {

  })
  .n("SuperpositionClient", "MoveContextCommand")
  .f(void 0, void 0)
  .ser(se_MoveContextCommand)
  .de(de_MoveContextCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: MoveContextInput;
      output: ContextActionResponse;
  };
  sdk: {
      input: MoveContextCommandInput;
      output: MoveContextCommandOutput;
  };
};
}
