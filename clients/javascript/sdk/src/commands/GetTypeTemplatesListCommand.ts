// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  GetTypeTemplatesListInput,
  GetTypeTemplatesListOutput,
} from "../models/models_0";
import {
  de_GetTypeTemplatesListCommand,
  se_GetTypeTemplatesListCommand,
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
 * The input for {@link GetTypeTemplatesListCommand}.
 */
export interface GetTypeTemplatesListCommandInput extends GetTypeTemplatesListInput {}
/**
 * @public
 *
 * The output of {@link GetTypeTemplatesListCommand}.
 */
export interface GetTypeTemplatesListCommandOutput extends GetTypeTemplatesListOutput, __MetadataBearer {}

/**
 * Retrieves a paginated list of all type templates in the workspace, including their schemas and metadata for type management.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetTypeTemplatesListCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetTypeTemplatesListCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetTypeTemplatesListInput
 *   count: Number("int"),
 *   page: Number("int"),
 *   all: true || false,
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 * };
 * const command = new GetTypeTemplatesListCommand(input);
 * const response = await client.send(command);
 * // { // GetTypeTemplatesListOutput
 * //   total_pages: Number("int"),
 * //   total_items: Number("int"),
 * //   data: [ // TypeTemplatesList
 * //     { // TypeTemplatesResponse
 * //       type_name: "STRING_VALUE", // required
 * //       type_schema: "DOCUMENT_VALUE", // required
 * //       description: "STRING_VALUE", // required
 * //       change_reason: "STRING_VALUE", // required
 * //       created_by: "STRING_VALUE", // required
 * //       created_at: new Date("TIMESTAMP"), // required
 * //       last_modified_at: new Date("TIMESTAMP"), // required
 * //       last_modified_by: "STRING_VALUE", // required
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param GetTypeTemplatesListCommandInput - {@link GetTypeTemplatesListCommandInput}
 * @returns {@link GetTypeTemplatesListCommandOutput}
 * @see {@link GetTypeTemplatesListCommandInput} for command's `input` shape.
 * @see {@link GetTypeTemplatesListCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class GetTypeTemplatesListCommand extends $Command.classBuilder<GetTypeTemplatesListCommandInput, GetTypeTemplatesListCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetTypeTemplatesList", {

  })
  .n("SuperpositionClient", "GetTypeTemplatesListCommand")
  .f(void 0, void 0)
  .ser(se_GetTypeTemplatesListCommand)
  .de(de_GetTypeTemplatesListCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetTypeTemplatesListInput;
      output: GetTypeTemplatesListOutput;
  };
  sdk: {
      input: GetTypeTemplatesListCommandInput;
      output: GetTypeTemplatesListCommandOutput;
  };
};
}
