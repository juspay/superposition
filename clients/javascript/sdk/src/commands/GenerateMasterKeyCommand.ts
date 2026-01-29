// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import { GenerateMasterKeyResponse } from "../models/models_0";
import {
  de_GenerateMasterKeyCommand,
  se_GenerateMasterKeyCommand,
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
 * The input for {@link GenerateMasterKeyCommand}.
 */
export interface GenerateMasterKeyCommandInput {}
/**
 * @public
 *
 * The output of {@link GenerateMasterKeyCommand}.
 */
export interface GenerateMasterKeyCommandOutput extends GenerateMasterKeyResponse, __MetadataBearer {}

/**
 * Generates a new master encryption key
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GenerateMasterKeyCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GenerateMasterKeyCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = {};
 * const command = new GenerateMasterKeyCommand(input);
 * const response = await client.send(command);
 * // { // GenerateMasterKeyResponse
 * //   master_key: "STRING_VALUE", // required
 * //   instructions: "STRING_VALUE", // required
 * //   warning: "STRING_VALUE", // required
 * // };
 *
 * ```
 *
 * @param GenerateMasterKeyCommandInput - {@link GenerateMasterKeyCommandInput}
 * @returns {@link GenerateMasterKeyCommandOutput}
 * @see {@link GenerateMasterKeyCommandInput} for command's `input` shape.
 * @see {@link GenerateMasterKeyCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class GenerateMasterKeyCommand extends $Command.classBuilder<GenerateMasterKeyCommandInput, GenerateMasterKeyCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GenerateMasterKey", {

  })
  .n("SuperpositionClient", "GenerateMasterKeyCommand")
  .f(void 0, void 0)
  .ser(se_GenerateMasterKeyCommand)
  .de(de_GenerateMasterKeyCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: {};
      output: GenerateMasterKeyResponse;
  };
  sdk: {
      input: GenerateMasterKeyCommandInput;
      output: GenerateMasterKeyCommandOutput;
  };
};
}
