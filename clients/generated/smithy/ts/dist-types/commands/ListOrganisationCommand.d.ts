import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { ListOrganisationInput, ListOrganisationOutput } from "../models/models_0";
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
export interface ListOrganisationCommandInput extends ListOrganisationInput {
}
/**
 * @public
 *
 * The output of {@link ListOrganisationCommand}.
 */
export interface ListOrganisationCommandOutput extends ListOrganisationOutput, __MetadataBearer {
}
declare const ListOrganisationCommand_base: {
    new (input: ListOrganisationCommandInput): import("@smithy/smithy-client").CommandImpl<ListOrganisationCommandInput, ListOrganisationCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (...[input]: [] | [ListOrganisationCommandInput]): import("@smithy/smithy-client").CommandImpl<ListOrganisationCommandInput, ListOrganisationCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListOrganisationCommand } from "@io.juspay/superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListOrganisationCommand } = require("@io.juspay/superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListOrganisationInput
 *   count: Number("int"),
 *   page: Number("int"),
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
 */
export declare class ListOrganisationCommand extends ListOrganisationCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
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
