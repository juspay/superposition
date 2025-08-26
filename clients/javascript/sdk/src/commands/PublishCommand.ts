// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  FunctionResponse,
  PublishInput,
} from "../models/models_0";
import {
  de_PublishCommand,
  se_PublishCommand,
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
 * The input for {@link PublishCommand}.
 */
export interface PublishCommandInput extends PublishInput {}
/**
 * @public
 *
 * The output of {@link PublishCommand}.
 */
export interface PublishCommandOutput extends FunctionResponse, __MetadataBearer {}

/**
 * Publishes the draft version of a function, making it the active version used for validation or autocompletion in the system.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, PublishCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, PublishCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // PublishInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   function_name: "STRING_VALUE", // required
 *   change_reason: "STRING_VALUE", // required
 * };
 * const command = new PublishCommand(input);
 * const response = await client.send(command);
 * // { // FunctionResponse
 * //   function_name: "STRING_VALUE", // required
 * //   published_code: "STRING_VALUE",
 * //   draft_code: "STRING_VALUE", // required
 * //   published_runtime_version: "STRING_VALUE",
 * //   draft_runtime_version: "STRING_VALUE", // required
 * //   published_at: new Date("TIMESTAMP"),
 * //   draft_edited_at: new Date("TIMESTAMP"), // required
 * //   published_by: "STRING_VALUE",
 * //   draft_edited_by: "STRING_VALUE", // required
 * //   last_modified_at: new Date("TIMESTAMP"), // required
 * //   last_modified_by: "STRING_VALUE", // required
 * //   change_reason: "STRING_VALUE", // required
 * //   description: "STRING_VALUE", // required
 * //   function_type: "VALIDATION" || "AUTOCOMPLETE", // required
 * // };
 *
 * ```
 *
 * @param PublishCommandInput - {@link PublishCommandInput}
 * @returns {@link PublishCommandOutput}
 * @see {@link PublishCommandInput} for command's `input` shape.
 * @see {@link PublishCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link FunctionNotFound} (client fault)
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class PublishCommand extends $Command.classBuilder<PublishCommandInput, PublishCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "Publish", {

  })
  .n("SuperpositionClient", "PublishCommand")
  .f(void 0, void 0)
  .ser(se_PublishCommand)
  .de(de_PublishCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: PublishInput;
      output: FunctionResponse;
  };
  sdk: {
      input: PublishCommandInput;
      output: PublishCommandOutput;
  };
};
}
