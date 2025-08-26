// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ContextResponse,
  UpdateOverrideInput,
} from "../models/models_0";
import {
  de_UpdateOverrideCommand,
  se_UpdateOverrideCommand,
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
 * The input for {@link UpdateOverrideCommand}.
 */
export interface UpdateOverrideCommandInput extends UpdateOverrideInput {}
/**
 * @public
 *
 * The output of {@link UpdateOverrideCommand}.
 */
export interface UpdateOverrideCommandOutput extends ContextResponse, __MetadataBearer {}

/**
 * Updates the overrides for an existing context. Allows modification of override values while maintaining the context's conditions.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, UpdateOverrideCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, UpdateOverrideCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // UpdateOverrideInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   config_tags: "STRING_VALUE",
 *   request: { // UpdateContextOverrideRequest
 *     context: { // ContextIdentifier Union: only one key present
 *       id: "STRING_VALUE",
 *       context: { // Condition
 *         "<keys>": "DOCUMENT_VALUE",
 *       },
 *     },
 *     override: { // Overrides // required
 *       "<keys>": "DOCUMENT_VALUE",
 *     },
 *     description: "STRING_VALUE",
 *     change_reason: "STRING_VALUE", // required
 *   },
 * };
 * const command = new UpdateOverrideCommand(input);
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
 * @param UpdateOverrideCommandInput - {@link UpdateOverrideCommandInput}
 * @returns {@link UpdateOverrideCommandOutput}
 * @see {@link UpdateOverrideCommandInput} for command's `input` shape.
 * @see {@link UpdateOverrideCommandOutput} for command's `response` shape.
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
export class UpdateOverrideCommand extends $Command.classBuilder<UpdateOverrideCommandInput, UpdateOverrideCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "UpdateOverride", {

  })
  .n("SuperpositionClient", "UpdateOverrideCommand")
  .f(void 0, void 0)
  .ser(se_UpdateOverrideCommand)
  .de(de_UpdateOverrideCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: UpdateOverrideInput;
      output: ContextResponse;
  };
  sdk: {
      input: UpdateOverrideCommandInput;
      output: UpdateOverrideCommandOutput;
  };
};
}
