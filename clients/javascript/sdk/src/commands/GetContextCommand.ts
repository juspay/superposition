// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ContextResponse,
  GetContextInput,
} from "../models/models_0";
import {
  de_GetContextCommand,
  se_GetContextCommand,
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
 * The input for {@link GetContextCommand}.
 */
export interface GetContextCommandInput extends GetContextInput {}
/**
 * @public
 *
 * The output of {@link GetContextCommand}.
 */
export interface GetContextCommandOutput extends ContextResponse, __MetadataBearer {}

/**
 * Retrieves detailed information about a specific context by its unique identifier, including conditions, overrides, and metadata.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetContextCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetContextCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetContextInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   id: "STRING_VALUE", // required
 * };
 * const command = new GetContextCommand(input);
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
 * @param GetContextCommandInput - {@link GetContextCommandInput}
 * @returns {@link GetContextCommandOutput}
 * @see {@link GetContextCommandInput} for command's `input` shape.
 * @see {@link GetContextCommandOutput} for command's `response` shape.
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
export class GetContextCommand extends $Command.classBuilder<GetContextCommandInput, GetContextCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetContext", {

  })
  .n("SuperpositionClient", "GetContextCommand")
  .f(void 0, void 0)
  .ser(se_GetContextCommand)
  .de(de_GetContextCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetContextInput;
      output: ContextResponse;
  };
  sdk: {
      input: GetContextCommandInput;
      output: GetContextCommandOutput;
  };
};
}
