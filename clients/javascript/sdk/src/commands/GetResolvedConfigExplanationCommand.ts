// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  GetResolvedConfigExplanationInput,
  GetResolvedConfigExplanationOutput,
} from "../models/models_0";
import {
  de_GetResolvedConfigExplanationCommand,
  se_GetResolvedConfigExplanationCommand,
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
 * The input for {@link GetResolvedConfigExplanationCommand}.
 */
export interface GetResolvedConfigExplanationCommandInput extends GetResolvedConfigExplanationInput {}
/**
 * @public
 *
 * The output of {@link GetResolvedConfigExplanationCommand}.
 */
export interface GetResolvedConfigExplanationCommandOutput extends GetResolvedConfigExplanationOutput, __MetadataBearer {}

/**
 * Explains how matching contexts affect a single resolved config key.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetResolvedConfigExplanationCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetResolvedConfigExplanationCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetResolvedConfigExplanationInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   key: "STRING_VALUE", // required
 *   version: "STRING_VALUE",
 *   merge_strategy: "MERGE" || "REPLACE",
 *   context_id: "STRING_VALUE",
 *   resolve_remote: true || false,
 *   context: { // ContextMap
 *     "<keys>": "DOCUMENT_VALUE",
 *   },
 * };
 * const command = new GetResolvedConfigExplanationCommand(input);
 * const response = await client.send(command);
 * // { // GetResolvedConfigExplanationOutput
 * //   explanation: { // ResolveExplanation
 * //     key: "STRING_VALUE", // required
 * //     timeline: [ // ResolveExplanationTimeline // required
 * //       { // ResolveExplanationTimelineItem
 * //         context_id: "STRING_VALUE", // required
 * //         condition: { // Condition // required
 * //           "<keys>": "DOCUMENT_VALUE",
 * //         },
 * //         override_id: "STRING_VALUE", // required
 * //         value_before: "DOCUMENT_VALUE", // required
 * //         value_after: "DOCUMENT_VALUE", // required
 * //       },
 * //     ],
 * //   },
 * //   version: "STRING_VALUE", // required
 * //   last_modified: new Date("TIMESTAMP"), // required
 * //   audit_id: "STRING_VALUE",
 * // };
 *
 * ```
 *
 * @param GetResolvedConfigExplanationCommandInput - {@link GetResolvedConfigExplanationCommandInput}
 * @returns {@link GetResolvedConfigExplanationCommandOutput}
 * @see {@link GetResolvedConfigExplanationCommandInput} for command's `input` shape.
 * @see {@link GetResolvedConfigExplanationCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class GetResolvedConfigExplanationCommand extends $Command.classBuilder<GetResolvedConfigExplanationCommandInput, GetResolvedConfigExplanationCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetResolvedConfigExplanation", {

  })
  .n("SuperpositionClient", "GetResolvedConfigExplanationCommand")
  .f(void 0, void 0)
  .ser(se_GetResolvedConfigExplanationCommand)
  .de(de_GetResolvedConfigExplanationCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetResolvedConfigExplanationInput;
      output: GetResolvedConfigExplanationOutput;
  };
  sdk: {
      input: GetResolvedConfigExplanationCommandInput;
      output: GetResolvedConfigExplanationCommandOutput;
  };
};
}
