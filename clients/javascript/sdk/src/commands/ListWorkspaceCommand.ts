// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ListWorkspaceInput,
  WorkspaceListResponse,
} from "../models/models_0";
import {
  de_ListWorkspaceCommand,
  se_ListWorkspaceCommand,
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
 * The input for {@link ListWorkspaceCommand}.
 */
export interface ListWorkspaceCommandInput extends ListWorkspaceInput {}
/**
 * @public
 *
 * The output of {@link ListWorkspaceCommand}.
 */
export interface ListWorkspaceCommandOutput extends WorkspaceListResponse, __MetadataBearer {}

/**
 * Retrieves a paginated list of all workspaces with optional filtering by workspace name, including their status, config details, and administrative information.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListWorkspaceCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListWorkspaceCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListWorkspaceInput
 *   count: Number("int"),
 *   page: Number("int"),
 *   all: true || false,
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
 * //       config_version: "STRING_VALUE",
 * //       created_by: "STRING_VALUE", // required
 * //       last_modified_by: "STRING_VALUE", // required
 * //       last_modified_at: new Date("TIMESTAMP"), // required
 * //       created_at: new Date("TIMESTAMP"), // required
 * //       mandatory_dimensions: [ // ListMandatoryDimensions
 * //         "STRING_VALUE",
 * //       ],
 * //       strict_mode: true || false, // required
 * //       metrics: "DOCUMENT_VALUE",
 * //       allow_experiment_self_approval: true || false, // required
 * //       auto_populate_control: true || false, // required
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
 * @public
 */
export class ListWorkspaceCommand extends $Command.classBuilder<ListWorkspaceCommandInput, ListWorkspaceCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "ListWorkspace", {

  })
  .n("SuperpositionClient", "ListWorkspaceCommand")
  .f(void 0, void 0)
  .ser(se_ListWorkspaceCommand)
  .de(de_ListWorkspaceCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
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
