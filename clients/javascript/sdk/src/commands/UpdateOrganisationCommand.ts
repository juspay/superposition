// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  OrganisationResponse,
  UpdateOrganisationRequest,
} from "../models/models_0";
import {
  de_UpdateOrganisationCommand,
  se_UpdateOrganisationCommand,
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
 * The input for {@link UpdateOrganisationCommand}.
 */
export interface UpdateOrganisationCommandInput extends UpdateOrganisationRequest {}
/**
 * @public
 *
 * The output of {@link UpdateOrganisationCommand}.
 */
export interface UpdateOrganisationCommandOutput extends OrganisationResponse, __MetadataBearer {}

/**
 * Updates an existing organisation's information including contact details, status, and administrative properties.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, UpdateOrganisationCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, UpdateOrganisationCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // UpdateOrganisationRequest
 *   country_code: "STRING_VALUE",
 *   contact_email: "STRING_VALUE",
 *   contact_phone: "STRING_VALUE",
 *   admin_email: "STRING_VALUE",
 *   sector: "STRING_VALUE",
 *   id: "STRING_VALUE", // required
 *   status: "Active" || "Inactive" || "PendingKyb",
 * };
 * const command = new UpdateOrganisationCommand(input);
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
 * @param UpdateOrganisationCommandInput - {@link UpdateOrganisationCommandInput}
 * @returns {@link UpdateOrganisationCommandOutput}
 * @see {@link UpdateOrganisationCommandInput} for command's `input` shape.
 * @see {@link UpdateOrganisationCommandOutput} for command's `response` shape.
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
export class UpdateOrganisationCommand extends $Command.classBuilder<UpdateOrganisationCommandInput, UpdateOrganisationCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "UpdateOrganisation", {

  })
  .n("SuperpositionClient", "UpdateOrganisationCommand")
  .f(void 0, void 0)
  .ser(se_UpdateOrganisationCommand)
  .de(de_UpdateOrganisationCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: UpdateOrganisationRequest;
      output: OrganisationResponse;
  };
  sdk: {
      input: UpdateOrganisationCommandInput;
      output: UpdateOrganisationCommandOutput;
  };
};
}
