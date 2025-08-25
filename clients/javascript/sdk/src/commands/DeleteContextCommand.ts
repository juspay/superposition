// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  DeleteContextInput,
  DeleteContextOutput,
} from "../models/models_0";
import {
  de_DeleteContextCommand,
  se_DeleteContextCommand,
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
 * The input for {@link DeleteContextCommand}.
 */
export interface DeleteContextCommandInput extends DeleteContextInput {}
/**
 * @public
 *
 * The output of {@link DeleteContextCommand}.
 */
export interface DeleteContextCommandOutput extends DeleteContextOutput, __MetadataBearer {}

/**
 * Permanently removes a context from the workspace. This operation cannot be undone and will affect config resolution.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, DeleteContextCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, DeleteContextCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // DeleteContextInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   id: "STRING_VALUE", // required
 *   config_tags: "STRING_VALUE",
 * };
 * const command = new DeleteContextCommand(input);
 * const response = await client.send(command);
 * // {};
 *
 * ```
 *
 * @param DeleteContextCommandInput - {@link DeleteContextCommandInput}
 * @returns {@link DeleteContextCommandOutput}
 * @see {@link DeleteContextCommandInput} for command's `input` shape.
 * @see {@link DeleteContextCommandOutput} for command's `response` shape.
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
export class DeleteContextCommand extends $Command.classBuilder<DeleteContextCommandInput, DeleteContextCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "DeleteContext", {

  })
  .n("SuperpositionClient", "DeleteContextCommand")
  .f(void 0, void 0)
  .ser(se_DeleteContextCommand)
  .de(de_DeleteContextCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: DeleteContextInput;
      output: {};
  };
  sdk: {
      input: DeleteContextCommandInput;
      output: DeleteContextCommandOutput;
  };
};
}
