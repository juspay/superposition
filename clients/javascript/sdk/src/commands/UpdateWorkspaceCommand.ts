// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  UpdateWorkspaceRequest,
  WorkspaceResponse,
} from "../models/models_0";
import {
  de_UpdateWorkspaceCommand,
  se_UpdateWorkspaceCommand,
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
 * The input for {@link UpdateWorkspaceCommand}.
 */
export interface UpdateWorkspaceCommandInput extends UpdateWorkspaceRequest {}
/**
 * @public
 *
 * The output of {@link UpdateWorkspaceCommand}.
 */
export interface UpdateWorkspaceCommandOutput extends WorkspaceResponse, __MetadataBearer {}

/**
 * Updates an existing workspace configuration, allowing modification of admin settings, mandatory dimensions, and workspace properties. Validates config version existence if provided.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, UpdateWorkspaceCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, UpdateWorkspaceCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // UpdateWorkspaceRequest
 *   org_id: "STRING_VALUE", // required
 *   workspace_name: "STRING_VALUE", // required
 *   workspace_admin_email: "STRING_VALUE", // required
 *   config_version: "STRING_VALUE",
 *   mandatory_dimensions: [ // ListMandatoryDimensions
 *     "STRING_VALUE",
 *   ],
 *   workspace_status: "ENABLED" || "DISABLED",
 *   metrics: "DOCUMENT_VALUE",
 *   allow_experiment_self_approval: true || false,
 *   auto_populate_control: true || false,
 * };
 * const command = new UpdateWorkspaceCommand(input);
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
 * //   strict_mode: true || false, // required
 * //   metrics: "DOCUMENT_VALUE",
 * //   allow_experiment_self_approval: true || false, // required
 * //   auto_populate_control: true || false, // required
 * // };
 *
 * ```
 *
 * @param UpdateWorkspaceCommandInput - {@link UpdateWorkspaceCommandInput}
 * @returns {@link UpdateWorkspaceCommandOutput}
 * @see {@link UpdateWorkspaceCommandInput} for command's `input` shape.
 * @see {@link UpdateWorkspaceCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link WorkspaceNotFound} (client fault)
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class UpdateWorkspaceCommand extends $Command.classBuilder<UpdateWorkspaceCommandInput, UpdateWorkspaceCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "UpdateWorkspace", {

  })
  .n("SuperpositionClient", "UpdateWorkspaceCommand")
  .f(void 0, void 0)
  .ser(se_UpdateWorkspaceCommand)
  .de(de_UpdateWorkspaceCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: UpdateWorkspaceRequest;
      output: WorkspaceResponse;
  };
  sdk: {
      input: UpdateWorkspaceCommandInput;
      output: UpdateWorkspaceCommandOutput;
  };
};
}
