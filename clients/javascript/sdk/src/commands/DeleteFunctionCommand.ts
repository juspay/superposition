// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  DeleteFunctionInput,
  DeleteFunctionOutput,
} from "../models/models_0";
import {
  de_DeleteFunctionCommand,
  se_DeleteFunctionCommand,
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
 * The input for {@link DeleteFunctionCommand}.
 */
export interface DeleteFunctionCommandInput extends DeleteFunctionInput {}
/**
 * @public
 *
 * The output of {@link DeleteFunctionCommand}.
 */
export interface DeleteFunctionCommandOutput extends DeleteFunctionOutput, __MetadataBearer {}

/**
 * Permanently removes a function from the workspace, deleting both draft and published versions along with all associated code. It fails if already in use
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, DeleteFunctionCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, DeleteFunctionCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // DeleteFunctionInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   function_name: "STRING_VALUE", // required
 * };
 * const command = new DeleteFunctionCommand(input);
 * const response = await client.send(command);
 * // {};
 *
 * ```
 *
 * @param DeleteFunctionCommandInput - {@link DeleteFunctionCommandInput}
 * @returns {@link DeleteFunctionCommandOutput}
 * @see {@link DeleteFunctionCommandInput} for command's `input` shape.
 * @see {@link DeleteFunctionCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link FunctionNotFound} (client fault)
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class DeleteFunctionCommand extends $Command.classBuilder<DeleteFunctionCommandInput, DeleteFunctionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "DeleteFunction", {

  })
  .n("SuperpositionClient", "DeleteFunctionCommand")
  .f(void 0, void 0)
  .ser(se_DeleteFunctionCommand)
  .de(de_DeleteFunctionCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: DeleteFunctionInput;
      output: {};
  };
  sdk: {
      input: DeleteFunctionCommandInput;
      output: DeleteFunctionCommandOutput;
  };
};
}
