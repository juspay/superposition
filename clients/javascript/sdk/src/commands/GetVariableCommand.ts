// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  GetVariableInput,
  VariableResponse,
} from "../models/models_0";
import {
  de_GetVariableCommand,
  se_GetVariableCommand,
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
 * The input for {@link GetVariableCommand}.
 */
export interface GetVariableCommandInput extends GetVariableInput {}
/**
 * @public
 *
 * The output of {@link GetVariableCommand}.
 */
export interface GetVariableCommandOutput extends VariableResponse, __MetadataBearer {}

/**
 * Retrieves detailed information about a specific variable by its name.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetVariableCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetVariableCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetVariableInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   name: "STRING_VALUE", // required
 * };
 * const command = new GetVariableCommand(input);
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
 * @param GetVariableCommandInput - {@link GetVariableCommandInput}
 * @returns {@link GetVariableCommandOutput}
 * @see {@link GetVariableCommandInput} for command's `input` shape.
 * @see {@link GetVariableCommandOutput} for command's `response` shape.
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
export class GetVariableCommand extends $Command.classBuilder<GetVariableCommandInput, GetVariableCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetVariable", {

  })
  .n("SuperpositionClient", "GetVariableCommand")
  .f(void 0, void 0)
  .ser(se_GetVariableCommand)
  .de(de_GetVariableCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetVariableInput;
      output: VariableResponse;
  };
  sdk: {
      input: GetVariableCommandInput;
      output: GetVariableCommandOutput;
  };
};
}
