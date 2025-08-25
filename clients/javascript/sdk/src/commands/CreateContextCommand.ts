// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ContextResponse,
  CreateContextInput,
} from "../models/models_0";
import {
  de_CreateContextCommand,
  se_CreateContextCommand,
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
 * The input for {@link CreateContextCommand}.
 */
export interface CreateContextCommandInput extends CreateContextInput {}
/**
 * @public
 *
 * The output of {@link CreateContextCommand}.
 */
export interface CreateContextCommandOutput extends ContextResponse, __MetadataBearer {}

/**
 * Creates a new context with specified conditions and overrides. Contexts define conditional rules for config management.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, CreateContextCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, CreateContextCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // CreateContextInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   context: { // Condition // required
 *     "<keys>": "DOCUMENT_VALUE",
 *   },
 *   config_tags: "STRING_VALUE",
 *   override: { // Overrides // required
 *     "<keys>": "DOCUMENT_VALUE",
 *   },
 *   description: "STRING_VALUE",
 *   change_reason: "STRING_VALUE", // required
 * };
 * const command = new CreateContextCommand(input);
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
 * @param CreateContextCommandInput - {@link CreateContextCommandInput}
 * @returns {@link CreateContextCommandOutput}
 * @see {@link CreateContextCommandInput} for command's `input` shape.
 * @see {@link CreateContextCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class CreateContextCommand extends $Command.classBuilder<CreateContextCommandInput, CreateContextCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "CreateContext", {

  })
  .n("SuperpositionClient", "CreateContextCommand")
  .f(void 0, void 0)
  .ser(se_CreateContextCommand)
  .de(de_CreateContextCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: CreateContextInput;
      output: ContextResponse;
  };
  sdk: {
      input: CreateContextCommandInput;
      output: CreateContextCommandOutput;
  };
};
}
