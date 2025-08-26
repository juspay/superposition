// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  FunctionResponse,
  UpdateFunctionRequest,
} from "../models/models_0";
import {
  de_UpdateFunctionCommand,
  se_UpdateFunctionCommand,
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
 * The input for {@link UpdateFunctionCommand}.
 */
export interface UpdateFunctionCommandInput extends UpdateFunctionRequest {}
/**
 * @public
 *
 * The output of {@link UpdateFunctionCommand}.
 */
export interface UpdateFunctionCommandOutput extends FunctionResponse, __MetadataBearer {}

/**
 * Updates the draft version of an existing function with new code, runtime version, or description while preserving the published version.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, UpdateFunctionCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, UpdateFunctionCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // UpdateFunctionRequest
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   function_name: "STRING_VALUE", // required
 *   description: "STRING_VALUE",
 *   change_reason: "STRING_VALUE", // required
 *   function: "STRING_VALUE", // required
 *   runtime_version: "STRING_VALUE", // required
 * };
 * const command = new UpdateFunctionCommand(input);
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
 * @param UpdateFunctionCommandInput - {@link UpdateFunctionCommandInput}
 * @returns {@link UpdateFunctionCommandOutput}
 * @see {@link UpdateFunctionCommandInput} for command's `input` shape.
 * @see {@link UpdateFunctionCommandOutput} for command's `response` shape.
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
export class UpdateFunctionCommand extends $Command.classBuilder<UpdateFunctionCommandInput, UpdateFunctionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "UpdateFunction", {

  })
  .n("SuperpositionClient", "UpdateFunctionCommand")
  .f(void 0, void 0)
  .ser(se_UpdateFunctionCommand)
  .de(de_UpdateFunctionCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: UpdateFunctionRequest;
      output: FunctionResponse;
  };
  sdk: {
      input: UpdateFunctionCommandInput;
      output: UpdateFunctionCommandOutput;
  };
};
}
