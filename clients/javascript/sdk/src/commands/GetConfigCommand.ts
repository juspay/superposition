// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  GetConfigInput,
  GetConfigOutput,
} from "../models/models_0";
import {
  de_GetConfigCommand,
  se_GetConfigCommand,
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
 * The input for {@link GetConfigCommand}.
 */
export interface GetConfigCommandInput extends GetConfigInput {}
/**
 * @public
 *
 * The output of {@link GetConfigCommand}.
 */
export interface GetConfigCommandOutput extends GetConfigOutput, __MetadataBearer {}

/**
 * Retrieves config data with context evaluation, including applicable contexts, overrides, and default values based on provided conditions.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetConfigCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetConfigCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetConfigInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   prefix: "STRING_VALUE",
 *   version: "STRING_VALUE",
 *   context: { // ContextMap
 *     "<keys>": "DOCUMENT_VALUE",
 *   },
 * };
 * const command = new GetConfigCommand(input);
 * const response = await client.send(command);
 * // { // GetConfigOutput
 * //   contexts: [ // ContextList
 * //     { // ContextPartial
 * //       id: "STRING_VALUE",
 * //       condition: { // Condition
 * //         "<keys>": "DOCUMENT_VALUE",
 * //       },
 * //       priority: Number("int"),
 * //       weight: Number("int"),
 * //       override_with_keys: [ // OverrideWithKeys
 * //         "STRING_VALUE",
 * //       ],
 * //     },
 * //   ],
 * //   overrides: { // OverridesMap
 * //     "<keys>": { // Overrides
 * //       "<keys>": "DOCUMENT_VALUE",
 * //     },
 * //   },
 * //   default_configs: { // Object
 * //     "<keys>": "DOCUMENT_VALUE",
 * //   },
 * //   version: "STRING_VALUE",
 * //   last_modified: new Date("TIMESTAMP"),
 * //   audit_id: "STRING_VALUE",
 * // };
 *
 * ```
 *
 * @param GetConfigCommandInput - {@link GetConfigCommandInput}
 * @returns {@link GetConfigCommandOutput}
 * @see {@link GetConfigCommandInput} for command's `input` shape.
 * @see {@link GetConfigCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class GetConfigCommand extends $Command.classBuilder<GetConfigCommandInput, GetConfigCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetConfig", {

  })
  .n("SuperpositionClient", "GetConfigCommand")
  .f(void 0, void 0)
  .ser(se_GetConfigCommand)
  .de(de_GetConfigCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetConfigInput;
      output: GetConfigOutput;
  };
  sdk: {
      input: GetConfigCommandInput;
      output: GetConfigCommandOutput;
  };
};
}
