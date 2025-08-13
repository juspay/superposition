// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  MigrateWorkspaceSchemaRequest,
  WorkspaceResponse,
} from "../models/models_0";
import {
  de_MigrateWorkspaceSchemaCommand,
  se_MigrateWorkspaceSchemaCommand,
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
 * The input for {@link MigrateWorkspaceSchemaCommand}.
 */
export interface MigrateWorkspaceSchemaCommandInput extends MigrateWorkspaceSchemaRequest {}
/**
 * @public
 *
 * The output of {@link MigrateWorkspaceSchemaCommand}.
 */
export interface MigrateWorkspaceSchemaCommandOutput extends WorkspaceResponse, __MetadataBearer {}

/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, MigrateWorkspaceSchemaCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, MigrateWorkspaceSchemaCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // MigrateWorkspaceSchemaRequest
 *   org_id: "STRING_VALUE", // required
 *   workspace_name: "STRING_VALUE", // required
 * };
 * const command = new MigrateWorkspaceSchemaCommand(input);
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
 * @param MigrateWorkspaceSchemaCommandInput - {@link MigrateWorkspaceSchemaCommandInput}
 * @returns {@link MigrateWorkspaceSchemaCommandOutput}
 * @see {@link MigrateWorkspaceSchemaCommandInput} for command's `input` shape.
 * @see {@link MigrateWorkspaceSchemaCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export class MigrateWorkspaceSchemaCommand extends $Command.classBuilder<MigrateWorkspaceSchemaCommandInput, MigrateWorkspaceSchemaCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "MigrateWorkspaceSchema", {

  })
  .n("SuperpositionClient", "MigrateWorkspaceSchemaCommand")
  .f(void 0, void 0)
  .ser(se_MigrateWorkspaceSchemaCommand)
  .de(de_MigrateWorkspaceSchemaCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: MigrateWorkspaceSchemaRequest;
      output: WorkspaceResponse;
  };
  sdk: {
      input: MigrateWorkspaceSchemaCommandInput;
      output: MigrateWorkspaceSchemaCommandOutput;
  };
};
}
