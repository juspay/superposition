// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ListOrganisationInput,
  ListOrganisationOutput,
} from "../models/models_0";
import {
  de_ListOrganisationCommand,
  se_ListOrganisationCommand,
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
 * The input for {@link ListOrganisationCommand}.
 */
export interface ListOrganisationCommandInput extends ListOrganisationInput {}
/**
 * @public
 *
 * The output of {@link ListOrganisationCommand}.
 */
export interface ListOrganisationCommandOutput extends ListOrganisationOutput, __MetadataBearer {}

/**
 * Retrieves a paginated list of all organisations with their basic information and status details.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListOrganisationCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListOrganisationCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListOrganisationInput
 *   count: Number("int"),
 *   page: Number("int"),
 *   all: true || false,
 * };
 * const command = new ListOrganisationCommand(input);
 * const response = await client.send(command);
 * // { // ListOrganisationOutput
 * //   total_pages: Number("int"),
 * //   total_items: Number("int"),
 * //   data: [ // OrganisationList
 * //     { // OrganisationResponse
 * //       id: "STRING_VALUE", // required
 * //       name: "STRING_VALUE", // required
 * //       country_code: "STRING_VALUE",
 * //       contact_email: "STRING_VALUE",
 * //       contact_phone: "STRING_VALUE",
 * //       created_by: "STRING_VALUE", // required
 * //       admin_email: "STRING_VALUE", // required
 * //       status: "Active" || "Inactive" || "PendingKyb", // required
 * //       sector: "STRING_VALUE",
 * //       created_at: new Date("TIMESTAMP"), // required
 * //       updated_at: new Date("TIMESTAMP"), // required
 * //       updated_by: "STRING_VALUE", // required
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param ListOrganisationCommandInput - {@link ListOrganisationCommandInput}
 * @returns {@link ListOrganisationCommandOutput}
 * @see {@link ListOrganisationCommandInput} for command's `input` shape.
 * @see {@link ListOrganisationCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class ListOrganisationCommand extends $Command.classBuilder<ListOrganisationCommandInput, ListOrganisationCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "ListOrganisation", {

  })
  .n("SuperpositionClient", "ListOrganisationCommand")
  .f(void 0, void 0)
  .ser(se_ListOrganisationCommand)
  .de(de_ListOrganisationCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: ListOrganisationInput;
      output: ListOrganisationOutput;
  };
  sdk: {
      input: ListOrganisationCommandInput;
      output: ListOrganisationCommandOutput;
  };
};
}
