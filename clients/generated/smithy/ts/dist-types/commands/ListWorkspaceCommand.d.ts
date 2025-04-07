import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { ListWorkspaceInput, WorkspaceListResponse } from "../models/models_0";
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
 * The input for {@link ListWorkspaceCommand}.
 */
export interface ListWorkspaceCommandInput extends ListWorkspaceInput {
}
/**
 * @public
 *
 * The output of {@link ListWorkspaceCommand}.
 */
export interface ListWorkspaceCommandOutput extends WorkspaceListResponse, __MetadataBearer {
}
declare const ListWorkspaceCommand_base: {
    new (input: ListWorkspaceCommandInput): import("@smithy/smithy-client").CommandImpl<ListWorkspaceCommandInput, ListWorkspaceCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: ListWorkspaceCommandInput): import("@smithy/smithy-client").CommandImpl<ListWorkspaceCommandInput, ListWorkspaceCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListWorkspaceCommand } from "@io.juspay/superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListWorkspaceCommand } = require("@io.juspay/superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListWorkspaceInput
 *   count: Number("int"),
 *   page: Number("int"),
 *   org_id: "STRING_VALUE", // required
 * };
 * const command = new ListWorkspaceCommand(input);
 * const response = await client.send(command);
 * // { // WorkspaceListResponse
 * //   total_pages: Number("long"), // required
 * //   total_items: Number("long"), // required
 * //   data: [ // WorkspaceList // required
 * //     { // WorkspaceResponse
 * //       workspace_name: "STRING_VALUE", // required
 * //       organisation_id: "STRING_VALUE", // required
 * //       organisation_name: "STRING_VALUE", // required
 * //       workspace_schema_name: "STRING_VALUE", // required
 * //       workspace_status: "ENABLED" || "DISABLED", // required
 * //       workspace_admin_email: "STRING_VALUE", // required
 * //       created_by: "STRING_VALUE", // required
 * //       last_modified_by: "STRING_VALUE", // required
 * //       last_modified_at: new Date("TIMESTAMP"), // required
 * //       created_at: new Date("TIMESTAMP"), // required
 * //       mandatory_dimensions: [ // ListMandatoryDimensions
 * //         "STRING_VALUE",
 * //       ],
 * //       workspace_strict_mode: true || false, // required
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param ListWorkspaceCommandInput - {@link ListWorkspaceCommandInput}
 * @returns {@link ListWorkspaceCommandOutput}
 * @see {@link ListWorkspaceCommandInput} for command's `input` shape.
 * @see {@link ListWorkspaceCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class ListWorkspaceCommand extends ListWorkspaceCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: ListWorkspaceInput;
            output: WorkspaceListResponse;
        };
        sdk: {
            input: ListWorkspaceCommandInput;
            output: ListWorkspaceCommandOutput;
        };
    };
}
