// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ListAuditLogsInput,
  ListAuditLogsOutput,
} from "../models/models_0";
import {
  de_ListAuditLogsCommand,
  se_ListAuditLogsCommand,
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
 * The input for {@link ListAuditLogsCommand}.
 */
export interface ListAuditLogsCommandInput extends ListAuditLogsInput {}
/**
 * @public
 *
 * The output of {@link ListAuditLogsCommand}.
 */
export interface ListAuditLogsCommandOutput extends ListAuditLogsOutput, __MetadataBearer {}

/**
 * Retrieves a paginated list of audit logs with support for filtering by date range, table names, actions, and usernames for compliance and monitoring purposes.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListAuditLogsCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListAuditLogsCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListAuditLogsInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   count: Number("int"),
 *   page: Number("int"),
 *   all: true || false,
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
 * @public
 */
export class ListAuditLogsCommand extends $Command.classBuilder<ListAuditLogsCommandInput, ListAuditLogsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "ListAuditLogs", {

  })
  .n("SuperpositionClient", "ListAuditLogsCommand")
  .f(void 0, void 0)
  .ser(se_ListAuditLogsCommand)
  .de(de_ListAuditLogsCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
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
