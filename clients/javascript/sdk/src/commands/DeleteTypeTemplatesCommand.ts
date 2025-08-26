// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  DeleteTypeTemplatesInput,
  TypeTemplatesResponse,
} from "../models/models_0";
import {
  de_DeleteTypeTemplatesCommand,
  se_DeleteTypeTemplatesCommand,
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
 * The input for {@link DeleteTypeTemplatesCommand}.
 */
export interface DeleteTypeTemplatesCommandInput extends DeleteTypeTemplatesInput {}
/**
 * @public
 *
 * The output of {@link DeleteTypeTemplatesCommand}.
 */
export interface DeleteTypeTemplatesCommandOutput extends TypeTemplatesResponse, __MetadataBearer {}

/**
 * Permanently removes a type template from the workspace. No checks performed while deleting
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, DeleteTypeTemplatesCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, DeleteTypeTemplatesCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // DeleteTypeTemplatesInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   type_name: "STRING_VALUE", // required
 * };
 * const command = new DeleteTypeTemplatesCommand(input);
 * const response = await client.send(command);
 * // { // TypeTemplatesResponse
 * //   type_name: "STRING_VALUE", // required
 * //   type_schema: "DOCUMENT_VALUE", // required
 * //   description: "STRING_VALUE", // required
 * //   change_reason: "STRING_VALUE", // required
 * //   created_by: "STRING_VALUE", // required
 * //   created_at: new Date("TIMESTAMP"), // required
 * //   last_modified_at: new Date("TIMESTAMP"), // required
 * //   last_modified_by: "STRING_VALUE", // required
 * // };
 *
 * ```
 *
 * @param DeleteTypeTemplatesCommandInput - {@link DeleteTypeTemplatesCommandInput}
 * @returns {@link DeleteTypeTemplatesCommandOutput}
 * @see {@link DeleteTypeTemplatesCommandInput} for command's `input` shape.
 * @see {@link DeleteTypeTemplatesCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link TypeTemplatesNotFound} (client fault)
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class DeleteTypeTemplatesCommand extends $Command.classBuilder<DeleteTypeTemplatesCommandInput, DeleteTypeTemplatesCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "DeleteTypeTemplates", {

  })
  .n("SuperpositionClient", "DeleteTypeTemplatesCommand")
  .f(void 0, void 0)
  .ser(se_DeleteTypeTemplatesCommand)
  .de(de_DeleteTypeTemplatesCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: DeleteTypeTemplatesInput;
      output: TypeTemplatesResponse;
  };
  sdk: {
      input: DeleteTypeTemplatesCommandInput;
      output: DeleteTypeTemplatesCommandOutput;
  };
};
}
