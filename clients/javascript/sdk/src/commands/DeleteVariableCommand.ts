// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  DeleteVariableInput,
  VariableResponse,
} from "../models/models_0";
import {
  de_DeleteVariableCommand,
  se_DeleteVariableCommand,
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
 * The input for {@link DeleteVariableCommand}.
 */
export interface DeleteVariableCommandInput extends DeleteVariableInput {}
/**
 * @public
 *
 * The output of {@link DeleteVariableCommand}.
 */
export interface DeleteVariableCommandOutput extends VariableResponse, __MetadataBearer {}

/**
 * Permanently deletes a variable from the workspace.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, DeleteVariableCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, DeleteVariableCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // DeleteVariableInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   name: "STRING_VALUE", // required
 * };
 * const command = new DeleteVariableCommand(input);
 * const response = await client.send(command);
 * // { // VariableResponse
 * //   name: "STRING_VALUE", // required
 * //   value: "STRING_VALUE", // required
 * //   description: "STRING_VALUE", // required
 * //   change_reason: "STRING_VALUE", // required
 * //   created_by: "STRING_VALUE", // required
 * //   created_at: new Date("TIMESTAMP"), // required
 * //   last_modified_by: "STRING_VALUE", // required
 * //   last_modified_at: new Date("TIMESTAMP"), // required
 * // };
 *
 * ```
 *
 * @param DeleteVariableCommandInput - {@link DeleteVariableCommandInput}
 * @returns {@link DeleteVariableCommandOutput}
 * @see {@link DeleteVariableCommandInput} for command's `input` shape.
 * @see {@link DeleteVariableCommandOutput} for command's `response` shape.
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
export class DeleteVariableCommand extends $Command.classBuilder<DeleteVariableCommandInput, DeleteVariableCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "DeleteVariable", {

  })
  .n("SuperpositionClient", "DeleteVariableCommand")
  .f(void 0, void 0)
  .ser(se_DeleteVariableCommand)
  .de(de_DeleteVariableCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: DeleteVariableInput;
      output: VariableResponse;
  };
  sdk: {
      input: DeleteVariableCommandInput;
      output: DeleteVariableCommandOutput;
  };
};
}
