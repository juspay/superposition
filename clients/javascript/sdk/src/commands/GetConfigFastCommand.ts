// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  GetConfigFastInput,
  GetConfigFastOutput,
} from "../models/models_0";
import {
  de_GetConfigFastCommand,
  se_GetConfigFastCommand,
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
 * The input for {@link GetConfigFastCommand}.
 */
export interface GetConfigFastCommandInput extends GetConfigFastInput {}
/**
 * @public
 *
 * The output of {@link GetConfigFastCommand}.
 */
export interface GetConfigFastCommandOutput extends GetConfigFastOutput, __MetadataBearer {}

/**
 * Retrieves the latest config with no processing for high-performance access.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetConfigFastCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetConfigFastCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetConfigFastInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 * };
 * const command = new GetConfigFastCommand(input);
 * const response = await client.send(command);
 * // { // GetConfigFastOutput
 * //   config: "DOCUMENT_VALUE",
 * //   version: "STRING_VALUE",
 * //   last_modified: new Date("TIMESTAMP"),
 * //   audit_id: "STRING_VALUE",
 * // };
 *
 * ```
 *
 * @param GetConfigFastCommandInput - {@link GetConfigFastCommandInput}
 * @returns {@link GetConfigFastCommandOutput}
 * @see {@link GetConfigFastCommandInput} for command's `input` shape.
 * @see {@link GetConfigFastCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class GetConfigFastCommand extends $Command.classBuilder<GetConfigFastCommandInput, GetConfigFastCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetConfigFast", {

  })
  .n("SuperpositionClient", "GetConfigFastCommand")
  .f(void 0, void 0)
  .ser(se_GetConfigFastCommand)
  .de(de_GetConfigFastCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetConfigFastInput;
      output: GetConfigFastOutput;
  };
  sdk: {
      input: GetConfigFastCommandInput;
      output: GetConfigFastCommandOutput;
  };
};
}
