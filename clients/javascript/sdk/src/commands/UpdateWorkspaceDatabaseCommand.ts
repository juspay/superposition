// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  UpdateWorkspaceDatabaseRequest,
  WorkspaceResponse,
} from "../models/models_0";
import {
  de_UpdateWorkspaceDatabaseCommand,
  se_UpdateWorkspaceDatabaseCommand,
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
 * The input for {@link UpdateWorkspaceDatabaseCommand}.
 */
export interface UpdateWorkspaceDatabaseCommandInput extends UpdateWorkspaceDatabaseRequest {}
/**
 * @public
 *
 * The output of {@link UpdateWorkspaceDatabaseCommand}.
 */
export interface UpdateWorkspaceDatabaseCommandOutput extends WorkspaceResponse, __MetadataBearer {}

/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, UpdateWorkspaceDatabaseCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, UpdateWorkspaceDatabaseCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // UpdateWorkspaceDatabaseRequest
 *   org_id: "STRING_VALUE", // required
 *   workspace_name: "STRING_VALUE", // required
 * };
 * const command = new UpdateWorkspaceDatabaseCommand(input);
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
 * @param UpdateWorkspaceDatabaseCommandInput - {@link UpdateWorkspaceDatabaseCommandInput}
 * @returns {@link UpdateWorkspaceDatabaseCommandOutput}
 * @see {@link UpdateWorkspaceDatabaseCommandInput} for command's `input` shape.
 * @see {@link UpdateWorkspaceDatabaseCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export class UpdateWorkspaceDatabaseCommand extends $Command.classBuilder<UpdateWorkspaceDatabaseCommandInput, UpdateWorkspaceDatabaseCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "UpdateWorkspaceDatabase", {

  })
  .n("SuperpositionClient", "UpdateWorkspaceDatabaseCommand")
  .f(void 0, void 0)
  .ser(se_UpdateWorkspaceDatabaseCommand)
  .de(de_UpdateWorkspaceDatabaseCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: UpdateWorkspaceDatabaseRequest;
      output: WorkspaceResponse;
  };
  sdk: {
      input: UpdateWorkspaceDatabaseCommandInput;
      output: UpdateWorkspaceDatabaseCommandOutput;
  };
};
}
