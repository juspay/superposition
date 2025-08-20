// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  FunctionResponse,
  GetFunctionInput,
} from "../models/models_0";
import {
  de_GetFunctionCommand,
  se_GetFunctionCommand,
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
 * The input for {@link GetFunctionCommand}.
 */
export interface GetFunctionCommandInput extends GetFunctionInput {}
/**
 * @public
 *
 * The output of {@link GetFunctionCommand}.
 */
export interface GetFunctionCommandOutput extends FunctionResponse, __MetadataBearer {}

/**
 * Retrieves detailed information about a specific function including its published and draft versions, code, and metadata.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetFunctionCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetFunctionCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetFunctionInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   function_name: "STRING_VALUE", // required
 * };
 * const command = new GetFunctionCommand(input);
 * const response = await client.send(command);
 * // { // FunctionResponse
 * //   function_name: "STRING_VALUE", // required
 * //   published_code: "STRING_VALUE",
 * //   draft_code: "STRING_VALUE", // required
 * //   published_runtime_version: "STRING_VALUE",
 * //   draft_runtime_version: "STRING_VALUE", // required
 * //   published_at: new Date("TIMESTAMP"),
 * //   draft_edited_at: new Date("TIMESTAMP"), // required
 * //   published_by: "STRING_VALUE",
 * //   draft_edited_by: "STRING_VALUE", // required
 * //   last_modified_at: new Date("TIMESTAMP"), // required
 * //   last_modified_by: "STRING_VALUE", // required
 * //   change_reason: "STRING_VALUE", // required
 * //   description: "STRING_VALUE", // required
 * //   function_type: "VALIDATION" || "AUTOCOMPLETE", // required
 * // };
 *
 * ```
 *
 * @param GetFunctionCommandInput - {@link GetFunctionCommandInput}
 * @returns {@link GetFunctionCommandOutput}
 * @see {@link GetFunctionCommandInput} for command's `input` shape.
 * @see {@link GetFunctionCommandOutput} for command's `response` shape.
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
export class GetFunctionCommand extends $Command.classBuilder<GetFunctionCommandInput, GetFunctionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetFunction", {

  })
  .n("SuperpositionClient", "GetFunctionCommand")
  .f(void 0, void 0)
  .ser(se_GetFunctionCommand)
  .de(de_GetFunctionCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetFunctionInput;
      output: FunctionResponse;
  };
  sdk: {
      input: GetFunctionCommandInput;
      output: GetFunctionCommandOutput;
  };
};
}
