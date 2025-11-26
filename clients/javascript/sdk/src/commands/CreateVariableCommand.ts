// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  CreateVariableInput,
  VariableResponse,
} from "../models/models_0";
import {
  de_CreateVariableCommand,
  se_CreateVariableCommand,
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
 * The input for {@link CreateVariableCommand}.
 */
export interface CreateVariableCommandInput extends CreateVariableInput {}
/**
 * @public
 *
 * The output of {@link CreateVariableCommand}.
 */
export interface CreateVariableCommandOutput extends VariableResponse, __MetadataBearer {}

/**
 * Creates a new variable with the specified name and value.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, CreateVariableCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, CreateVariableCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // CreateVariableInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   name: "STRING_VALUE", // required
 *   value: "STRING_VALUE", // required
 *   description: "STRING_VALUE", // required
 *   change_reason: "STRING_VALUE", // required
 * };
 * const command = new CreateVariableCommand(input);
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
 * @param CreateVariableCommandInput - {@link CreateVariableCommandInput}
 * @returns {@link CreateVariableCommandOutput}
 * @see {@link CreateVariableCommandInput} for command's `input` shape.
 * @see {@link CreateVariableCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class CreateVariableCommand extends $Command.classBuilder<CreateVariableCommandInput, CreateVariableCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "CreateVariable", {

  })
  .n("SuperpositionClient", "CreateVariableCommand")
  .f(void 0, void 0)
  .ser(se_CreateVariableCommand)
  .de(de_CreateVariableCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: CreateVariableInput;
      output: VariableResponse;
  };
  sdk: {
      input: CreateVariableCommandInput;
      output: CreateVariableCommandOutput;
  };
};
}
