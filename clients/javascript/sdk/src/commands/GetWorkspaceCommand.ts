// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  GetWorkspaceInput,
  WorkspaceResponse,
} from "../models/models_0";
import {
  de_GetWorkspaceCommand,
  se_GetWorkspaceCommand,
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
 * The input for {@link GetWorkspaceCommand}.
 */
export interface GetWorkspaceCommandInput extends GetWorkspaceInput {}
/**
 * @public
 *
 * The output of {@link GetWorkspaceCommand}.
 */
export interface GetWorkspaceCommandOutput extends WorkspaceResponse, __MetadataBearer {}

/**
 * Retrieves detailed information about a specific workspace including its configuration and metadata.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetWorkspaceCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetWorkspaceCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetWorkspaceInput
 *   org_id: "STRING_VALUE", // required
 *   workspace_name: "STRING_VALUE", // required
 * };
 * const command = new GetWorkspaceCommand(input);
 * const response = await client.send(command);
 * // { // WorkspaceResponse
 * //   workspace_name: "STRING_VALUE", // required
 * //   organisation_id: "STRING_VALUE", // required
 * //   organisation_name: "STRING_VALUE", // required
 * //   workspace_schema_name: "STRING_VALUE", // required
 * //   workspace_status: "ENABLED" || "DISABLED", // required
 * //   workspace_admin_email: "STRING_VALUE", // required
 * //   config_version: "STRING_VALUE",
 * //   created_by: "STRING_VALUE", // required
 * //   last_modified_by: "STRING_VALUE", // required
 * //   last_modified_at: new Date("TIMESTAMP"), // required
 * //   created_at: new Date("TIMESTAMP"), // required
 * //   mandatory_dimensions: [ // ListMandatoryDimensions
 * //     "STRING_VALUE",
 * //   ],
 * //   metrics: "DOCUMENT_VALUE", // required
 * //   allow_experiment_self_approval: true || false, // required
 * //   auto_populate_control: true || false, // required
 * //   enable_context_validation: true || false, // required
 * //   enable_change_reason_validation: true || false, // required
 * // };
 *
 * ```
 *
 * @param GetWorkspaceCommandInput - {@link GetWorkspaceCommandInput}
 * @returns {@link GetWorkspaceCommandOutput}
 * @see {@link GetWorkspaceCommandInput} for command's `input` shape.
 * @see {@link GetWorkspaceCommandOutput} for command's `response` shape.
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
export class GetWorkspaceCommand extends $Command.classBuilder<GetWorkspaceCommandInput, GetWorkspaceCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetWorkspace", {

  })
  .n("SuperpositionClient", "GetWorkspaceCommand")
  .f(void 0, void 0)
  .ser(se_GetWorkspaceCommand)
  .de(de_GetWorkspaceCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetWorkspaceInput;
      output: WorkspaceResponse;
  };
  sdk: {
      input: GetWorkspaceCommandInput;
      output: GetWorkspaceCommandOutput;
  };
};
}
