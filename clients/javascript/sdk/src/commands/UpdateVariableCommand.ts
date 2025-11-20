// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  UpdateVariableInput,
  VariableResponse,
} from "../models/models_0";
import {
  de_UpdateVariableCommand,
  se_UpdateVariableCommand,
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
 * The input for {@link UpdateVariableCommand}.
 */
export interface UpdateVariableCommandInput extends UpdateVariableInput {}
/**
 * @public
 *
 * The output of {@link UpdateVariableCommand}.
 */
export interface UpdateVariableCommandOutput extends VariableResponse, __MetadataBearer {}

/**
 * Updates an existing variable's value, description, or tags.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, UpdateVariableCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, UpdateVariableCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // UpdateVariableInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   name: "STRING_VALUE", // required
 *   value: "STRING_VALUE",
 *   description: "STRING_VALUE",
 *   change_reason: "STRING_VALUE", // required
 * };
 * const command = new UpdateVariableCommand(input);
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
 * @param UpdateVariableCommandInput - {@link UpdateVariableCommandInput}
 * @returns {@link UpdateVariableCommandOutput}
 * @see {@link UpdateVariableCommandInput} for command's `input` shape.
 * @see {@link UpdateVariableCommandOutput} for command's `response` shape.
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
export class UpdateVariableCommand extends $Command.classBuilder<UpdateVariableCommandInput, UpdateVariableCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "UpdateVariable", {

  })
  .n("SuperpositionClient", "UpdateVariableCommand")
  .f(void 0, void 0)
  .ser(se_UpdateVariableCommand)
  .de(de_UpdateVariableCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: UpdateVariableInput;
      output: VariableResponse;
  };
  sdk: {
      input: UpdateVariableCommandInput;
      output: UpdateVariableCommandOutput;
  };
};
}
