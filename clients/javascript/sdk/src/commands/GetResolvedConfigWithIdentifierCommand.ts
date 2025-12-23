// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  GetResolvedConfigWithIdentifierInput,
  GetResolvedConfigWithIdentifierOutput,
} from "../models/models_0";
import {
  de_GetResolvedConfigWithIdentifierCommand,
  se_GetResolvedConfigWithIdentifierCommand,
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
 * The input for {@link GetResolvedConfigWithIdentifierCommand}.
 */
export interface GetResolvedConfigWithIdentifierCommandInput extends GetResolvedConfigWithIdentifierInput {}
/**
 * @public
 *
 * The output of {@link GetResolvedConfigWithIdentifierCommand}.
 */
export interface GetResolvedConfigWithIdentifierCommandOutput extends GetResolvedConfigWithIdentifierOutput, __MetadataBearer {}

/**
 * Resolves and merges config values based on context conditions and identifier, applying overrides and merge strategies to produce the final configuration.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetResolvedConfigWithIdentifierCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetResolvedConfigWithIdentifierCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetResolvedConfigWithIdentifierInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   prefix: [ // StringList
 *     "STRING_VALUE",
 *   ],
 *   version: "STRING_VALUE",
 *   show_reasoning: true || false,
 *   merge_strategy: "MERGE" || "REPLACE",
 *   context_id: "STRING_VALUE",
 *   resolve_remote: true || false,
 *   context: { // ContextMap
 *     "<keys>": "DOCUMENT_VALUE",
 *   },
 *   identifier: "STRING_VALUE",
 * };
 * const command = new GetResolvedConfigWithIdentifierCommand(input);
 * const response = await client.send(command);
 * // { // GetResolvedConfigWithIdentifierOutput
 * //   config: "DOCUMENT_VALUE", // required
 * //   version: "STRING_VALUE", // required
 * //   last_modified: new Date("TIMESTAMP"), // required
 * //   audit_id: "STRING_VALUE",
 * // };
 *
 * ```
 *
 * @param GetResolvedConfigWithIdentifierCommandInput - {@link GetResolvedConfigWithIdentifierCommandInput}
 * @returns {@link GetResolvedConfigWithIdentifierCommandOutput}
 * @see {@link GetResolvedConfigWithIdentifierCommandInput} for command's `input` shape.
 * @see {@link GetResolvedConfigWithIdentifierCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class GetResolvedConfigWithIdentifierCommand extends $Command.classBuilder<GetResolvedConfigWithIdentifierCommandInput, GetResolvedConfigWithIdentifierCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetResolvedConfigWithIdentifier", {

  })
  .n("SuperpositionClient", "GetResolvedConfigWithIdentifierCommand")
  .f(void 0, void 0)
  .ser(se_GetResolvedConfigWithIdentifierCommand)
  .de(de_GetResolvedConfigWithIdentifierCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetResolvedConfigWithIdentifierInput;
      output: GetResolvedConfigWithIdentifierOutput;
  };
  sdk: {
      input: GetResolvedConfigWithIdentifierCommandInput;
      output: GetResolvedConfigWithIdentifierCommandOutput;
  };
};
}
