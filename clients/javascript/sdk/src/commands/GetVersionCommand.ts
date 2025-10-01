// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  GetVersionInput,
  GetVersionResponse,
} from "../models/models_0";
import {
  de_GetVersionCommand,
  se_GetVersionCommand,
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
 * The input for {@link GetVersionCommand}.
 */
export interface GetVersionCommandInput extends GetVersionInput {}
/**
 * @public
 *
 * The output of {@link GetVersionCommand}.
 */
export interface GetVersionCommandOutput extends GetVersionResponse, __MetadataBearer {}

/**
 * Retrieves a specific config version along with its metadata for audit and rollback purposes.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetVersionCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetVersionCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetVersionInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   id: "STRING_VALUE", // required
 * };
 * const command = new GetVersionCommand(input);
 * const response = await client.send(command);
 * // { // GetVersionResponse
 * //   id: "STRING_VALUE", // required
 * //   config: "DOCUMENT_VALUE", // required
 * //   config_hash: "STRING_VALUE", // required
 * //   created_at: new Date("TIMESTAMP"), // required
 * //   description: "STRING_VALUE", // required
 * //   tags: [ // StringList
 * //     "STRING_VALUE",
 * //   ],
 * // };
 *
 * ```
 *
 * @param GetVersionCommandInput - {@link GetVersionCommandInput}
 * @returns {@link GetVersionCommandOutput}
 * @see {@link GetVersionCommandInput} for command's `input` shape.
 * @see {@link GetVersionCommandOutput} for command's `response` shape.
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
export class GetVersionCommand extends $Command.classBuilder<GetVersionCommandInput, GetVersionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetVersion", {

  })
  .n("SuperpositionClient", "GetVersionCommand")
  .f(void 0, void 0)
  .ser(se_GetVersionCommand)
  .de(de_GetVersionCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetVersionInput;
      output: GetVersionResponse;
  };
  sdk: {
      input: GetVersionCommandInput;
      output: GetVersionCommandOutput;
  };
};
}
