// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  FunctionExecutionResponse,
  TestInput,
} from "../models/models_0";
import {
  de_TestCommand,
  se_TestCommand,
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
 * The input for {@link TestCommand}.
 */
export interface TestCommandInput extends TestInput {}
/**
 * @public
 *
 * The output of {@link TestCommand}.
 */
export interface TestCommandOutput extends FunctionExecutionResponse, __MetadataBearer {}

/**
 * Executes a function in test mode with provided input parameters to validate its behavior before publishing or deployment.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, TestCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, TestCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // TestInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   function_name: "STRING_VALUE", // required
 *   stage: "draft" || "published", // required
 *   request: { // FunctionExecutionRequest Union: only one key present
 *     ValidateFunctionRequest: { // ValidateFunctionRequest
 *       key: "STRING_VALUE",
 *       value: "DOCUMENT_VALUE",
 *     },
 *     AutocompleteFunctionRequest: { // AutocompleteFunctionRequest
 *       name: "STRING_VALUE",
 *       prefix: "STRING_VALUE",
 *       environment: "DOCUMENT_VALUE",
 *     },
 *   },
 * };
 * const command = new TestCommand(input);
 * const response = await client.send(command);
 * // { // FunctionExecutionResponse
 * //   fn_output: "DOCUMENT_VALUE", // required
 * //   stdout: "STRING_VALUE", // required
 * //   function_type: "VALIDATION" || "AUTOCOMPLETE", // required
 * // };
 *
 * ```
 *
 * @param TestCommandInput - {@link TestCommandInput}
 * @returns {@link TestCommandOutput}
 * @see {@link TestCommandInput} for command's `input` shape.
 * @see {@link TestCommandOutput} for command's `response` shape.
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
export class TestCommand extends $Command.classBuilder<TestCommandInput, TestCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "Test", {

  })
  .n("SuperpositionClient", "TestCommand")
  .f(void 0, void 0)
  .ser(se_TestCommand)
  .de(de_TestCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: TestInput;
      output: FunctionExecutionResponse;
  };
  sdk: {
      input: TestCommandInput;
      output: TestCommandOutput;
  };
};
}
