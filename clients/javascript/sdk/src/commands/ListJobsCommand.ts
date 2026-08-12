// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ListJobsInput,
  ListJobsOutput,
} from "../models/models_0";
import {
  de_ListJobsCommand,
  se_ListJobsCommand,
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
 * The input for {@link ListJobsCommand}.
 */
export interface ListJobsCommandInput extends ListJobsInput {}
/**
 * @public
 *
 * The output of {@link ListJobsCommand}.
 */
export interface ListJobsCommandOutput extends ListJobsOutput, __MetadataBearer {}

/**
 * Retrieves a paginated list of background jobs in the workspace, optionally filtered by type and status.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListJobsCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListJobsCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListJobsInput
 *   count: Number("int"),
 *   page: Number("int"),
 *   all: true || false,
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   status: "CREATED" || "SCHEDULED" || "INPROGRESS" || "FAILED" || "COMPLETED",
 *   job_type: "WEBHOOK" || "PRIORITY_RECOMPUTE" || "REDUCE",
 * };
 * const command = new ListJobsCommand(input);
 * const response = await client.send(command);
 * // { // ListJobsOutput
 * //   total_pages: Number("int"), // required
 * //   total_items: Number("int"), // required
 * //   data: [ // JobList // required
 * //     { // JobDetailResponse
 * //       id: "STRING_VALUE", // required
 * //       kronos_job_id: "STRING_VALUE", // required
 * //       description: "STRING_VALUE", // required
 * //       job_type: "WEBHOOK" || "PRIORITY_RECOMPUTE" || "REDUCE", // required
 * //       status: "CREATED" || "SCHEDULED" || "INPROGRESS" || "FAILED" || "COMPLETED", // required
 * //       name: "STRING_VALUE", // required
 * //       progress: Number("int"), // required
 * //       workspace_schema: "STRING_VALUE", // required
 * //       created_at: new Date("TIMESTAMP"), // required
 * //       logs: "DOCUMENT_VALUE", // required
 * //       execution: { // ExecutionDetails
 * //         attempt_count: Number("long"),
 * //         max_attempts: Number("long"),
 * //         started_at: new Date("TIMESTAMP"),
 * //         completed_at: new Date("TIMESTAMP"),
 * //         duration_ms: Number("long"),
 * //         execution_status: "STRING_VALUE",
 * //       },
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param ListJobsCommandInput - {@link ListJobsCommandInput}
 * @returns {@link ListJobsCommandOutput}
 * @see {@link ListJobsCommandInput} for command's `input` shape.
 * @see {@link ListJobsCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class ListJobsCommand extends $Command.classBuilder<ListJobsCommandInput, ListJobsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "ListJobs", {

  })
  .n("SuperpositionClient", "ListJobsCommand")
  .f(void 0, void 0)
  .ser(se_ListJobsCommand)
  .de(de_ListJobsCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: ListJobsInput;
      output: ListJobsOutput;
  };
  sdk: {
      input: ListJobsCommandInput;
      output: ListJobsCommandOutput;
  };
};
}
