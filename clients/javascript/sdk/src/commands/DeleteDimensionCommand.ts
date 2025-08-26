// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  DeleteDimensionInput,
  DeleteDimensionOutput,
} from "../models/models_0";
import {
  de_DeleteDimensionCommand,
  se_DeleteDimensionCommand,
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
 * The input for {@link DeleteDimensionCommand}.
 */
export interface DeleteDimensionCommandInput extends DeleteDimensionInput {}
/**
 * @public
 *
 * The output of {@link DeleteDimensionCommand}.
 */
export interface DeleteDimensionCommandOutput extends DeleteDimensionOutput, __MetadataBearer {}

/**
 * Permanently removes a dimension from the workspace. This operation will fail if the dimension has active dependencies or is referenced by existing configurations.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, DeleteDimensionCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, DeleteDimensionCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // DeleteDimensionInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   dimension: "STRING_VALUE", // required
 * };
 * const command = new DeleteDimensionCommand(input);
 * const response = await client.send(command);
 * // {};
 *
 * ```
 *
 * @param DeleteDimensionCommandInput - {@link DeleteDimensionCommandInput}
 * @returns {@link DeleteDimensionCommandOutput}
 * @see {@link DeleteDimensionCommandInput} for command's `input` shape.
 * @see {@link DeleteDimensionCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link ResourceNotFound} (client fault)
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class DeleteDimensionCommand extends $Command.classBuilder<DeleteDimensionCommandInput, DeleteDimensionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "DeleteDimension", {

  })
  .n("SuperpositionClient", "DeleteDimensionCommand")
  .f(void 0, void 0)
  .ser(se_DeleteDimensionCommand)
  .de(de_DeleteDimensionCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: DeleteDimensionInput;
      output: {};
  };
  sdk: {
      input: DeleteDimensionCommandInput;
      output: DeleteDimensionCommandOutput;
  };
};
}
