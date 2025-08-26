// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  TypeTemplatesResponse,
  UpdateTypeTemplatesRequest,
} from "../models/models_0";
import {
  de_UpdateTypeTemplatesCommand,
  se_UpdateTypeTemplatesCommand,
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
 * The input for {@link UpdateTypeTemplatesCommand}.
 */
export interface UpdateTypeTemplatesCommandInput extends UpdateTypeTemplatesRequest {}
/**
 * @public
 *
 * The output of {@link UpdateTypeTemplatesCommand}.
 */
export interface UpdateTypeTemplatesCommandOutput extends TypeTemplatesResponse, __MetadataBearer {}

/**
 * Updates an existing type template's schema definition and metadata while preserving its identifier and usage history.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, UpdateTypeTemplatesCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, UpdateTypeTemplatesCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // UpdateTypeTemplatesRequest
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   type_name: "STRING_VALUE", // required
 *   type_schema: "DOCUMENT_VALUE", // required
 *   description: "STRING_VALUE",
 *   change_reason: "STRING_VALUE", // required
 * };
 * const command = new UpdateTypeTemplatesCommand(input);
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
 * @param UpdateTypeTemplatesCommandInput - {@link UpdateTypeTemplatesCommandInput}
 * @returns {@link UpdateTypeTemplatesCommandOutput}
 * @see {@link UpdateTypeTemplatesCommandInput} for command's `input` shape.
 * @see {@link UpdateTypeTemplatesCommandOutput} for command's `response` shape.
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
export class UpdateTypeTemplatesCommand extends $Command.classBuilder<UpdateTypeTemplatesCommandInput, UpdateTypeTemplatesCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "UpdateTypeTemplates", {

  })
  .n("SuperpositionClient", "UpdateTypeTemplatesCommand")
  .f(void 0, void 0)
  .ser(se_UpdateTypeTemplatesCommand)
  .de(de_UpdateTypeTemplatesCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: UpdateTypeTemplatesRequest;
      output: TypeTemplatesResponse;
  };
  sdk: {
      input: UpdateTypeTemplatesCommandInput;
      output: UpdateTypeTemplatesCommandOutput;
  };
};
}
