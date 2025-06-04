// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  CreateWorkspaceRequest,
  WorkspaceResponse,
} from "../models/models_0";
import {
  de_CreateWorkspaceCommand,
  se_CreateWorkspaceCommand,
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
 * The input for {@link CreateWorkspaceCommand}.
 */
export interface CreateWorkspaceCommandInput extends CreateWorkspaceRequest {}
/**
 * @public
 *
 * The output of {@link CreateWorkspaceCommand}.
 */
export interface CreateWorkspaceCommandOutput extends WorkspaceResponse, __MetadataBearer {}

/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, CreateWorkspaceCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, CreateWorkspaceCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // CreateWorkspaceRequest
 *   org_id: "STRING_VALUE", // required
 *   workspace_admin_email: "STRING_VALUE", // required
 *   workspace_name: "STRING_VALUE", // required
 *   workspace_status: "ENABLED" || "DISABLED",
 *   workspace_strict_mode: true || false, // required
 * };
 * const command = new CreateWorkspaceCommand(input);
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
 * //   workspace_strict_mode: true || false, // required
 * // };
 *
 * ```
 *
 * @param CreateWorkspaceCommandInput - {@link CreateWorkspaceCommandInput}
 * @returns {@link CreateWorkspaceCommandOutput}
 * @see {@link CreateWorkspaceCommandInput} for command's `input` shape.
 * @see {@link CreateWorkspaceCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export class CreateWorkspaceCommand extends $Command.classBuilder<CreateWorkspaceCommandInput, CreateWorkspaceCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "CreateWorkspace", {

  })
  .n("SuperpositionClient", "CreateWorkspaceCommand")
  .f(void 0, void 0)
  .ser(se_CreateWorkspaceCommand)
  .de(de_CreateWorkspaceCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: CreateWorkspaceRequest;
      output: WorkspaceResponse;
  };
  sdk: {
      input: CreateWorkspaceCommandInput;
      output: CreateWorkspaceCommandOutput;
  };
};
}
