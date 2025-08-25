// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  GetResolvedConfigInput,
  GetResolvedConfigOutput,
} from "../models/models_0";
import {
  de_GetResolvedConfigCommand,
  se_GetResolvedConfigCommand,
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
 * The input for {@link GetResolvedConfigCommand}.
 */
export interface GetResolvedConfigCommandInput extends GetResolvedConfigInput {}
/**
 * @public
 *
 * The output of {@link GetResolvedConfigCommand}.
 */
export interface GetResolvedConfigCommandOutput extends GetResolvedConfigOutput, __MetadataBearer {}

/**
 * Resolves and merges config values based on context conditions, applying overrides and merge strategies to produce the final configuration.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetResolvedConfigCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetResolvedConfigCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetResolvedConfigInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   prefix: "STRING_VALUE",
 *   version: "STRING_VALUE",
 *   show_reasoning: true || false,
 *   merge_strategy: "MERGE" || "REPLACE",
 *   context_id: "STRING_VALUE",
 *   context: { // ContextMap
 *     "<keys>": "DOCUMENT_VALUE",
 *   },
 * };
 * const command = new GetResolvedConfigCommand(input);
 * const response = await client.send(command);
 * // { // GetResolvedConfigOutput
 * //   config: "DOCUMENT_VALUE",
 * //   version: "STRING_VALUE",
 * //   last_modified: new Date("TIMESTAMP"),
 * //   audit_id: "STRING_VALUE",
 * // };
 *
 * ```
 *
 * @param GetResolvedConfigCommandInput - {@link GetResolvedConfigCommandInput}
 * @returns {@link GetResolvedConfigCommandOutput}
 * @see {@link GetResolvedConfigCommandInput} for command's `input` shape.
 * @see {@link GetResolvedConfigCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class GetResolvedConfigCommand extends $Command.classBuilder<GetResolvedConfigCommandInput, GetResolvedConfigCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetResolvedConfig", {

  })
  .n("SuperpositionClient", "GetResolvedConfigCommand")
  .f(void 0, void 0)
  .ser(se_GetResolvedConfigCommand)
  .de(de_GetResolvedConfigCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetResolvedConfigInput;
      output: GetResolvedConfigOutput;
  };
  sdk: {
      input: GetResolvedConfigCommandInput;
      output: GetResolvedConfigCommandOutput;
  };
};
}
