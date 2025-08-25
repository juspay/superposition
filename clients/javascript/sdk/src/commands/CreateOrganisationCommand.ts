// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  CreateOrganisationRequest,
  OrganisationResponse,
} from "../models/models_0";
import {
  de_CreateOrganisationCommand,
  se_CreateOrganisationCommand,
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
 * The input for {@link CreateOrganisationCommand}.
 */
export interface CreateOrganisationCommandInput extends CreateOrganisationRequest {}
/**
 * @public
 *
 * The output of {@link CreateOrganisationCommand}.
 */
export interface CreateOrganisationCommandOutput extends OrganisationResponse, __MetadataBearer {}

/**
 * Creates a new organisation with specified details including name, admin contact, and organisational information.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, CreateOrganisationCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, CreateOrganisationCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // CreateOrganisationRequest
 *   country_code: "STRING_VALUE",
 *   contact_email: "STRING_VALUE",
 *   contact_phone: "STRING_VALUE",
 *   admin_email: "STRING_VALUE", // required
 *   sector: "STRING_VALUE",
 *   name: "STRING_VALUE", // required
 * };
 * const command = new CreateOrganisationCommand(input);
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
 * @param CreateOrganisationCommandInput - {@link CreateOrganisationCommandInput}
 * @returns {@link CreateOrganisationCommandOutput}
 * @see {@link CreateOrganisationCommandInput} for command's `input` shape.
 * @see {@link CreateOrganisationCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class CreateOrganisationCommand extends $Command.classBuilder<CreateOrganisationCommandInput, CreateOrganisationCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "CreateOrganisation", {

  })
  .n("SuperpositionClient", "CreateOrganisationCommand")
  .f(void 0, void 0)
  .ser(se_CreateOrganisationCommand)
  .de(de_CreateOrganisationCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: CreateOrganisationRequest;
      output: OrganisationResponse;
  };
  sdk: {
      input: CreateOrganisationCommandInput;
      output: CreateOrganisationCommandOutput;
  };
};
}
