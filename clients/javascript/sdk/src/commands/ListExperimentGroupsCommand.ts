// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ListExperimentGroupsInput,
  ListExperimentGroupsResponse,
} from "../models/models_0";
import {
  de_ListExperimentGroupsCommand,
  se_ListExperimentGroupsCommand,
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
 * The input for {@link ListExperimentGroupsCommand}.
 */
export interface ListExperimentGroupsCommandInput extends ListExperimentGroupsInput {}
/**
 * @public
 *
 * The output of {@link ListExperimentGroupsCommand}.
 */
export interface ListExperimentGroupsCommandOutput extends ListExperimentGroupsResponse, __MetadataBearer {}

/**
 * Lists experiment groups, with support for filtering and pagination.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListExperimentGroupsCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListExperimentGroupsCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListExperimentGroupsInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   page: Number("long"),
 *   count: Number("long"),
 *   name: "STRING_VALUE",
 *   created_by: "STRING_VALUE",
 *   last_modified_by: "STRING_VALUE",
 *   sort_on: "name" || "created_at" || "last_modified_at",
 *   sort_by: "desc" || "asc",
 *   all: true || false,
 * };
 * const command = new ListExperimentGroupsCommand(input);
 * const response = await client.send(command);
 * // { // ListExperimentGroupsResponse
 * //   total_pages: Number("long"), // required
 * //   total_items: Number("long"), // required
 * //   data: [ // ExperimentGroupList // required
 * //     { // ExperimentGroupResponse
 * //       id: "STRING_VALUE", // required
 * //       context_hash: "STRING_VALUE", // required
 * //       name: "STRING_VALUE", // required
 * //       description: "STRING_VALUE", // required
 * //       change_reason: "STRING_VALUE", // required
 * //       context: { // Condition // required
 * //         "<keys>": "DOCUMENT_VALUE",
 * //       },
 * //       traffic_percentage: Number("int"), // required
 * //       member_experiment_ids: [ // StringList // required
 * //         "STRING_VALUE",
 * //       ],
 * //       created_at: new Date("TIMESTAMP"), // required
 * //       created_by: "STRING_VALUE", // required
 * //       last_modified_at: new Date("TIMESTAMP"), // required
 * //       last_modified_by: "STRING_VALUE", // required
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param ListExperimentGroupsCommandInput - {@link ListExperimentGroupsCommandInput}
 * @returns {@link ListExperimentGroupsCommandOutput}
 * @see {@link ListExperimentGroupsCommandInput} for command's `input` shape.
 * @see {@link ListExperimentGroupsCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class ListExperimentGroupsCommand extends $Command.classBuilder<ListExperimentGroupsCommandInput, ListExperimentGroupsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "ListExperimentGroups", {

  })
  .n("SuperpositionClient", "ListExperimentGroupsCommand")
  .f(void 0, void 0)
  .ser(se_ListExperimentGroupsCommand)
  .de(de_ListExperimentGroupsCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: ListExperimentGroupsInput;
      output: ListExperimentGroupsResponse;
  };
  sdk: {
      input: ListExperimentGroupsCommandInput;
      output: ListExperimentGroupsCommandOutput;
  };
};
}
