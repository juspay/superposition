// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  GetOrganisationInput,
  OrganisationResponse,
} from "../models/models_0";
import {
  de_GetOrganisationCommand,
  se_GetOrganisationCommand,
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
 * The input for {@link GetOrganisationCommand}.
 */
export interface GetOrganisationCommandInput extends GetOrganisationInput {}
/**
 * @public
 *
 * The output of {@link GetOrganisationCommand}.
 */
export interface GetOrganisationCommandOutput extends OrganisationResponse, __MetadataBearer {}

/**
 * Retrieves detailed information about a specific organisation including its status, contact details, and administrative metadata.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetOrganisationCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetOrganisationCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetOrganisationInput
 *   id: "STRING_VALUE", // required
 * };
 * const command = new GetOrganisationCommand(input);
 * const response = await client.send(command);
 * // { // OrganisationResponse
 * //   id: "STRING_VALUE", // required
 * //   name: "STRING_VALUE", // required
 * //   country_code: "STRING_VALUE",
 * //   contact_email: "STRING_VALUE",
 * //   contact_phone: "STRING_VALUE",
 * //   created_by: "STRING_VALUE", // required
 * //   admin_email: "STRING_VALUE", // required
 * //   status: "Active" || "Inactive" || "PendingKyb", // required
 * //   sector: "STRING_VALUE",
 * //   created_at: new Date("TIMESTAMP"), // required
 * //   updated_at: new Date("TIMESTAMP"), // required
 * //   updated_by: "STRING_VALUE", // required
 * // };
 *
 * ```
 *
 * @param GetOrganisationCommandInput - {@link GetOrganisationCommandInput}
 * @returns {@link GetOrganisationCommandOutput}
 * @see {@link GetOrganisationCommandInput} for command's `input` shape.
 * @see {@link GetOrganisationCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link OrganisationNotFound} (client fault)
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class GetOrganisationCommand extends $Command.classBuilder<GetOrganisationCommandInput, GetOrganisationCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetOrganisation", {

  })
  .n("SuperpositionClient", "GetOrganisationCommand")
  .f(void 0, void 0)
  .ser(se_GetOrganisationCommand)
  .de(de_GetOrganisationCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetOrganisationInput;
      output: OrganisationResponse;
  };
  sdk: {
      input: GetOrganisationCommandInput;
      output: GetOrganisationCommandOutput;
  };
};
}
