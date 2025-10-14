// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  GetTypeTemplateInput,
  TypeTemplatesResponse,
} from "../models/models_0";
import {
  de_GetTypeTemplateCommand,
  se_GetTypeTemplateCommand,
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
 * The input for {@link GetTypeTemplateCommand}.
 */
export interface GetTypeTemplateCommandInput extends GetTypeTemplateInput {}
/**
 * @public
 *
 * The output of {@link GetTypeTemplateCommand}.
 */
export interface GetTypeTemplateCommandOutput extends TypeTemplatesResponse, __MetadataBearer {}

/**
 * Retrieves detailed information about a specific type template including its schema and metadata.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetTypeTemplateCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetTypeTemplateCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetTypeTemplateInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   type_name: "STRING_VALUE", // required
 * };
 * const command = new GetTypeTemplateCommand(input);
 * const response = await client.send(command);
 * // { // TypeTemplatesResponse
 * //   type_name: "STRING_VALUE", // required
 * //   type_schema: { // Object // required
 * //     "<keys>": "DOCUMENT_VALUE",
 * //   },
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
 * @param GetTypeTemplateCommandInput - {@link GetTypeTemplateCommandInput}
 * @returns {@link GetTypeTemplateCommandOutput}
 * @see {@link GetTypeTemplateCommandInput} for command's `input` shape.
 * @see {@link GetTypeTemplateCommandOutput} for command's `response` shape.
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
export class GetTypeTemplateCommand extends $Command.classBuilder<GetTypeTemplateCommandInput, GetTypeTemplateCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetTypeTemplate", {

  })
  .n("SuperpositionClient", "GetTypeTemplateCommand")
  .f(void 0, void 0)
  .ser(se_GetTypeTemplateCommand)
  .de(de_GetTypeTemplateCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetTypeTemplateInput;
      output: TypeTemplatesResponse;
  };
  sdk: {
      input: GetTypeTemplateCommandInput;
      output: GetTypeTemplateCommandOutput;
  };
};
}
