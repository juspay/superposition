// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  GetJobInput,
  JobDetailResponse,
} from "../models/models_0";
import {
  de_GetJobCommand,
  se_GetJobCommand,
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
 * The input for {@link GetJobCommand}.
 */
export interface GetJobCommandInput extends GetJobInput {}
/**
 * @public
 *
 * The output of {@link GetJobCommand}.
 */
export interface GetJobCommandOutput extends JobDetailResponse, __MetadataBearer {}

/**
 * Retrieves detailed information about a specific background job, including Kronos execution details such as attempt count, timing, and duration.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetJobCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetJobCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetJobInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   id: "STRING_VALUE", // required
 * };
 * const command = new GetJobCommand(input);
 * const response = await client.send(command);
 * // { // JobDetailResponse
 * //   id: "STRING_VALUE", // required
 * //   kronos_job_id: "STRING_VALUE", // required
 * //   description: "STRING_VALUE", // required
 * //   job_type: "WEBHOOK" || "PRIORITY_RECOMPUTE" || "REDUCE", // required
 * //   status: "CREATED" || "SCHEDULED" || "INPROGRESS" || "FAILED" || "COMPLETED", // required
 * //   name: "STRING_VALUE", // required
 * //   progress: Number("int"), // required
 * //   workspace_schema: "STRING_VALUE", // required
 * //   created_at: new Date("TIMESTAMP"), // required
 * //   logs: "DOCUMENT_VALUE", // required
 * //   execution: { // ExecutionDetails
 * //     attempt_count: Number("long"),
 * //     max_attempts: Number("long"),
 * //     started_at: new Date("TIMESTAMP"),
 * //     completed_at: new Date("TIMESTAMP"),
 * //     duration_ms: Number("long"),
 * //     execution_status: "STRING_VALUE",
 * //   },
 * // };
 *
 * ```
 *
 * @param GetJobCommandInput - {@link GetJobCommandInput}
 * @returns {@link GetJobCommandOutput}
 * @see {@link GetJobCommandInput} for command's `input` shape.
 * @see {@link GetJobCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link ResourceNotFound} (client fault)
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class GetJobCommand extends $Command.classBuilder<GetJobCommandInput, GetJobCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetJob", {

  })
  .n("SuperpositionClient", "GetJobCommand")
  .f(void 0, void 0)
  .ser(se_GetJobCommand)
  .de(de_GetJobCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetJobInput;
      output: JobDetailResponse;
  };
  sdk: {
      input: GetJobCommandInput;
      output: GetJobCommandOutput;
  };
};
}
