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
  de_CreaterOrganisationCommand,
  se_CreaterOrganisationCommand,
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
 * The input for {@link CreaterOrganisationCommand}.
 */
export interface CreaterOrganisationCommandInput extends CreateOrganisationRequest {}
/**
 * @public
 *
 * The output of {@link CreaterOrganisationCommand}.
 */
export interface CreaterOrganisationCommandOutput extends OrganisationResponse, __MetadataBearer {}

/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, CreaterOrganisationCommand } from "@io.juspay/superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, CreaterOrganisationCommand } = require("@io.juspay/superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // CreateOrganisationRequest
 *   country_code: "STRING_VALUE",
 *   contact_email: "STRING_VALUE",
 *   contact_phone: "STRING_VALUE",
 *   admin_email: "STRING_VALUE", // required
 *   name: "STRING_VALUE", // required
 *   sector: "STRING_VALUE",
 * };
 * const command = new CreaterOrganisationCommand(input);
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
 * @param CreaterOrganisationCommandInput - {@link CreaterOrganisationCommandInput}
 * @returns {@link CreaterOrganisationCommandOutput}
 * @see {@link CreaterOrganisationCommandInput} for command's `input` shape.
 * @see {@link CreaterOrganisationCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export class CreaterOrganisationCommand extends $Command.classBuilder<CreaterOrganisationCommandInput, CreaterOrganisationCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "CreaterOrganisation", {

  })
  .n("SuperpositionClient", "CreaterOrganisationCommand")
  .f(void 0, void 0)
  .ser(se_CreaterOrganisationCommand)
  .de(de_CreaterOrganisationCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: CreateOrganisationRequest;
      output: OrganisationResponse;
  };
  sdk: {
      input: CreaterOrganisationCommandInput;
      output: CreaterOrganisationCommandOutput;
  };
};
}
