import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { ListAuditLogsInput, ListAuditLogsOutput } from "../models/models_0";
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
 * The input for {@link ListAuditLogsCommand}.
 */
export interface ListAuditLogsCommandInput extends ListAuditLogsInput {
}
/**
 * @public
 *
 * The output of {@link ListAuditLogsCommand}.
 */
export interface ListAuditLogsCommandOutput extends ListAuditLogsOutput, __MetadataBearer {
}
declare const ListAuditLogsCommand_base: {
    new (input: ListAuditLogsCommandInput): import("@smithy/smithy-client").CommandImpl<ListAuditLogsCommandInput, ListAuditLogsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: ListAuditLogsCommandInput): import("@smithy/smithy-client").CommandImpl<ListAuditLogsCommandInput, ListAuditLogsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListAuditLogsCommand } from "@io.juspay/superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListAuditLogsCommand } = require("@io.juspay/superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListAuditLogsInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   count: Number("int"),
 *   page: Number("int"),
 *   from_date: new Date("TIMESTAMP"),
 *   to_date: new Date("TIMESTAMP"),
 *   tables: "STRING_VALUE",
 *   action: "STRING_VALUE",
 *   username: "STRING_VALUE",
 * };
 * const command = new ListAuditLogsCommand(input);
 * const response = await client.send(command);
 * // { // ListAuditLogsOutput
 * //   total_pages: Number("int"),
 * //   total_items: Number("int"),
 * //   data: [ // AuditLogList
 * //     { // AuditLogFull
 * //       table_name: "STRING_VALUE",
 * //       user_name: "STRING_VALUE",
 * //       timestamp: new Date("TIMESTAMP"),
 * //       action: "STRING_VALUE",
 * //       original_data: "DOCUMENT_VALUE",
 * //       new_data: "DOCUMENT_VALUE",
 * //       query: "STRING_VALUE",
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param ListAuditLogsCommandInput - {@link ListAuditLogsCommandInput}
 * @returns {@link ListAuditLogsCommandOutput}
 * @see {@link ListAuditLogsCommandInput} for command's `input` shape.
 * @see {@link ListAuditLogsCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class ListAuditLogsCommand extends ListAuditLogsCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: ListAuditLogsInput;
            output: ListAuditLogsOutput;
        };
        sdk: {
            input: ListAuditLogsCommandInput;
            output: ListAuditLogsCommandOutput;
        };
    };
}
