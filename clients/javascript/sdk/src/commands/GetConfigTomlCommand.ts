// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  GetConfigTomlInput,
  GetConfigTomlOutput,
} from "../models/models_0";
import {
  de_GetConfigTomlCommand,
  se_GetConfigTomlCommand,
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
 * The input for {@link GetConfigTomlCommand}.
 */
export interface GetConfigTomlCommandInput extends GetConfigTomlInput {}
/**
 * @public
 *
 * The output of {@link GetConfigTomlCommand}.
 */
export interface GetConfigTomlCommandOutput extends GetConfigTomlOutput, __MetadataBearer {}

/**
 * Retrieves the full config in TOML format, including default configs with schemas, dimensions, and overrides. This endpoint is optimized for clients that prefer TOML format for configuration management.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetConfigTomlCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetConfigTomlCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetConfigTomlInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 * };
 * const command = new GetConfigTomlCommand(input);
 * const response = await client.send(command);
 * // { // GetConfigTomlOutput
 * //   toml_config: "STRING_VALUE", // required
 * //   last_modified: new Date("TIMESTAMP"),
 * // };
 *
 * ```
 *
 * @param GetConfigTomlCommandInput - {@link GetConfigTomlCommandInput}
 * @returns {@link GetConfigTomlCommandOutput}
 * @see {@link GetConfigTomlCommandInput} for command's `input` shape.
 * @see {@link GetConfigTomlCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class GetConfigTomlCommand extends $Command.classBuilder<GetConfigTomlCommandInput, GetConfigTomlCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetConfigToml", {

  })
  .n("SuperpositionClient", "GetConfigTomlCommand")
  .f(void 0, void 0)
  .ser(se_GetConfigTomlCommand)
  .de(de_GetConfigTomlCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetConfigTomlInput;
      output: GetConfigTomlOutput;
  };
  sdk: {
      input: GetConfigTomlCommandInput;
      output: GetConfigTomlCommandOutput;
  };
};
}
