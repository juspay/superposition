// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  GetConfigJsonInput,
  GetConfigJsonOutput,
} from "../models/models_0";
import {
  de_GetConfigJsonCommand,
  se_GetConfigJsonCommand,
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
 * The input for {@link GetConfigJsonCommand}.
 */
export interface GetConfigJsonCommandInput extends GetConfigJsonInput {}
/**
 * @public
 *
 * The output of {@link GetConfigJsonCommand}.
 */
export interface GetConfigJsonCommandOutput extends GetConfigJsonOutput, __MetadataBearer {}

/**
 * Retrieves the full config in JSON format, including default configs with schemas, dimensions, and overrides. This endpoint is optimized for clients that prefer JSON format for configuration management.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetConfigJsonCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetConfigJsonCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetConfigJsonInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 * };
 * const command = new GetConfigJsonCommand(input);
 * const response = await client.send(command);
 * // { // GetConfigJsonOutput
 * //   json_config: "STRING_VALUE", // required
 * //   last_modified: new Date("TIMESTAMP"),
 * // };
 *
 * ```
 *
 * @param GetConfigJsonCommandInput - {@link GetConfigJsonCommandInput}
 * @returns {@link GetConfigJsonCommandOutput}
 * @see {@link GetConfigJsonCommandInput} for command's `input` shape.
 * @see {@link GetConfigJsonCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class GetConfigJsonCommand extends $Command.classBuilder<GetConfigJsonCommandInput, GetConfigJsonCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetConfigJson", {

  })
  .n("SuperpositionClient", "GetConfigJsonCommand")
  .f(void 0, void 0)
  .ser(se_GetConfigJsonCommand)
  .de(de_GetConfigJsonCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetConfigJsonInput;
      output: GetConfigJsonOutput;
  };
  sdk: {
      input: GetConfigJsonCommandInput;
      output: GetConfigJsonCommandOutput;
  };
};
}
