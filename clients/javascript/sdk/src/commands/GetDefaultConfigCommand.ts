// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  DefaultConfigResponse,
  GetDefaultConfigInput,
} from "../models/models_0";
import {
  de_GetDefaultConfigCommand,
  se_GetDefaultConfigCommand,
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
 * The input for {@link GetDefaultConfigCommand}.
 */
export interface GetDefaultConfigCommandInput extends GetDefaultConfigInput {}
/**
 * @public
 *
 * The output of {@link GetDefaultConfigCommand}.
 */
export interface GetDefaultConfigCommandOutput extends DefaultConfigResponse, __MetadataBearer {}

/**
 * Retrieves a specific default config entry by its key, including its value, schema, function mappings, and metadata.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetDefaultConfigCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetDefaultConfigCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetDefaultConfigInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   key: "STRING_VALUE", // required
 * };
 * const command = new GetDefaultConfigCommand(input);
 * const response = await client.send(command);
 * // { // DefaultConfigResponse
 * //   key: "STRING_VALUE", // required
 * //   value: "DOCUMENT_VALUE", // required
 * //   schema: { // Object // required
 * //     "<keys>": "DOCUMENT_VALUE",
 * //   },
 * //   description: "STRING_VALUE", // required
 * //   change_reason: "STRING_VALUE", // required
 * //   value_validation_function_name: "STRING_VALUE",
 * //   value_compute_function_name: "STRING_VALUE",
 * //   created_at: new Date("TIMESTAMP"), // required
 * //   created_by: "STRING_VALUE", // required
 * //   last_modified_at: new Date("TIMESTAMP"), // required
 * //   last_modified_by: "STRING_VALUE", // required
 * // };
 *
 * ```
 *
 * @param GetDefaultConfigCommandInput - {@link GetDefaultConfigCommandInput}
 * @returns {@link GetDefaultConfigCommandOutput}
 * @see {@link GetDefaultConfigCommandInput} for command's `input` shape.
 * @see {@link GetDefaultConfigCommandOutput} for command's `response` shape.
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
export class GetDefaultConfigCommand extends $Command.classBuilder<GetDefaultConfigCommandInput, GetDefaultConfigCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetDefaultConfig", {

  })
  .n("SuperpositionClient", "GetDefaultConfigCommand")
  .f(void 0, void 0)
  .ser(se_GetDefaultConfigCommand)
  .de(de_GetDefaultConfigCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetDefaultConfigInput;
      output: DefaultConfigResponse;
  };
  sdk: {
      input: GetDefaultConfigCommandInput;
      output: GetDefaultConfigCommandOutput;
  };
};
}
