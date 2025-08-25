// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ContextResponse,
  GetContextFromConditionInput,
} from "../models/models_0";
import {
  de_GetContextFromConditionCommand,
  se_GetContextFromConditionCommand,
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
 * The input for {@link GetContextFromConditionCommand}.
 */
export interface GetContextFromConditionCommandInput extends GetContextFromConditionInput {}
/**
 * @public
 *
 * The output of {@link GetContextFromConditionCommand}.
 */
export interface GetContextFromConditionCommandOutput extends ContextResponse, __MetadataBearer {}

/**
 * Retrieves context information by matching against provided conditions. Used to find contexts that would apply to specific scenarios.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetContextFromConditionCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetContextFromConditionCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetContextFromConditionInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   context: "DOCUMENT_VALUE",
 * };
 * const command = new GetContextFromConditionCommand(input);
 * const response = await client.send(command);
 * // { // ContextResponse
 * //   id: "STRING_VALUE", // required
 * //   value: { // Condition
 * //     "<keys>": "DOCUMENT_VALUE",
 * //   },
 * //   override: { // Overrides
 * //     "<keys>": "DOCUMENT_VALUE",
 * //   },
 * //   override_id: "STRING_VALUE",
 * //   weight: "STRING_VALUE",
 * //   description: "STRING_VALUE",
 * //   change_reason: "STRING_VALUE",
 * //   created_at: new Date("TIMESTAMP"),
 * //   created_by: "STRING_VALUE",
 * //   last_modified_at: new Date("TIMESTAMP"),
 * //   last_modified_by: "STRING_VALUE",
 * // };
 *
 * ```
 *
 * @param GetContextFromConditionCommandInput - {@link GetContextFromConditionCommandInput}
 * @returns {@link GetContextFromConditionCommandOutput}
 * @see {@link GetContextFromConditionCommandInput} for command's `input` shape.
 * @see {@link GetContextFromConditionCommandOutput} for command's `response` shape.
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
export class GetContextFromConditionCommand extends $Command.classBuilder<GetContextFromConditionCommandInput, GetContextFromConditionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetContextFromCondition", {

  })
  .n("SuperpositionClient", "GetContextFromConditionCommand")
  .f(void 0, void 0)
  .ser(se_GetContextFromConditionCommand)
  .de(de_GetContextFromConditionCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetContextFromConditionInput;
      output: ContextResponse;
  };
  sdk: {
      input: GetContextFromConditionCommandInput;
      output: GetContextFromConditionCommandOutput;
  };
};
}
