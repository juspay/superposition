// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  GetDetailedResolvedConfigInput,
  GetDetailedResolvedConfigOutput,
} from "../models/models_0";
import {
  de_GetDetailedResolvedConfigCommand,
  se_GetDetailedResolvedConfigCommand,
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
 * The input for {@link GetDetailedResolvedConfigCommand}.
 */
export interface GetDetailedResolvedConfigCommandInput extends GetDetailedResolvedConfigInput {}
/**
 * @public
 *
 * The output of {@link GetDetailedResolvedConfigCommand}.
 */
export interface GetDetailedResolvedConfigCommandOutput extends GetDetailedResolvedConfigOutput, __MetadataBearer {}

/**
 * Resolves config values and returns each key with default-config metadata.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetDetailedResolvedConfigCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetDetailedResolvedConfigCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetDetailedResolvedConfigInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   prefix: [ // StringList
 *     "STRING_VALUE",
 *   ],
 *   exclude_prefix: [
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
 * };
 * const command = new GetDetailedResolvedConfigCommand(input);
 * const response = await client.send(command);
 * // { // GetDetailedResolvedConfigOutput
 * //   config: "DOCUMENT_VALUE", // required
 * //   version: "STRING_VALUE", // required
 * //   last_modified: new Date("TIMESTAMP"), // required
 * //   audit_id: "STRING_VALUE",
 * // };
 *
 * ```
 *
 * @param GetDetailedResolvedConfigCommandInput - {@link GetDetailedResolvedConfigCommandInput}
 * @returns {@link GetDetailedResolvedConfigCommandOutput}
 * @see {@link GetDetailedResolvedConfigCommandInput} for command's `input` shape.
 * @see {@link GetDetailedResolvedConfigCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class GetDetailedResolvedConfigCommand extends $Command.classBuilder<GetDetailedResolvedConfigCommandInput, GetDetailedResolvedConfigCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetDetailedResolvedConfig", {

  })
  .n("SuperpositionClient", "GetDetailedResolvedConfigCommand")
  .f(void 0, void 0)
  .ser(se_GetDetailedResolvedConfigCommand)
  .de(de_GetDetailedResolvedConfigCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetDetailedResolvedConfigInput;
      output: GetDetailedResolvedConfigOutput;
  };
  sdk: {
      input: GetDetailedResolvedConfigCommandInput;
      output: GetDetailedResolvedConfigCommandOutput;
  };
};
}
