// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ImportConfigOutput,
  ImportConfigTomlInput,
} from "../models/models_0";
import {
  de_ImportConfigTomlCommand,
  se_ImportConfigTomlCommand,
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
 * The input for {@link ImportConfigTomlCommand}.
 */
export interface ImportConfigTomlCommandInput extends ImportConfigTomlInput {}
/**
 * @public
 *
 * The output of {@link ImportConfigTomlCommand}.
 */
export interface ImportConfigTomlCommandOutput extends ImportConfigOutput, __MetadataBearer {}

/**
 * Imports a full config from a TOML document, persisting dimensions, default-configs and contexts in a single transaction after validating the document.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ImportConfigTomlCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ImportConfigTomlCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ImportConfigTomlInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   strategy: "create_only" || "upsert" || "replace",
 *   on_error: "abort" || "continue",
 *   dry_run: true || false,
 *   config_tags: "STRING_VALUE",
 *   toml_config: "STRING_VALUE", // required
 * };
 * const command = new ImportConfigTomlCommand(input);
 * const response = await client.send(command);
 * // { // ImportConfigOutput
 * //   strategy: "STRING_VALUE", // required
 * //   dry_run: true || false, // required
 * //   config_version: "STRING_VALUE",
 * //   dimensions: { // ImportEntityReport
 * //     created: Number("int"), // required
 * //     updated: Number("int"), // required
 * //     skipped: Number("int"), // required
 * //     deleted: Number("int"), // required
 * //     errors: [ // ImportErrorList
 * //       { // ImportErrorItem
 * //         id: "STRING_VALUE", // required
 * //         message: "STRING_VALUE", // required
 * //       },
 * //     ],
 * //   },
 * //   default_configs: {
 * //     created: Number("int"), // required
 * //     updated: Number("int"), // required
 * //     skipped: Number("int"), // required
 * //     deleted: Number("int"), // required
 * //     errors: [
 * //       {
 * //         id: "STRING_VALUE", // required
 * //         message: "STRING_VALUE", // required
 * //       },
 * //     ],
 * //   },
 * //   contexts: {
 * //     created: Number("int"), // required
 * //     updated: Number("int"), // required
 * //     skipped: Number("int"), // required
 * //     deleted: Number("int"), // required
 * //     errors: [
 * //       {
 * //         id: "STRING_VALUE", // required
 * //         message: "STRING_VALUE", // required
 * //       },
 * //     ],
 * //   },
 * // };
 *
 * ```
 *
 * @param ImportConfigTomlCommandInput - {@link ImportConfigTomlCommandInput}
 * @returns {@link ImportConfigTomlCommandOutput}
 * @see {@link ImportConfigTomlCommandInput} for command's `input` shape.
 * @see {@link ImportConfigTomlCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class ImportConfigTomlCommand extends $Command.classBuilder<ImportConfigTomlCommandInput, ImportConfigTomlCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "ImportConfigToml", {

  })
  .n("SuperpositionClient", "ImportConfigTomlCommand")
  .f(void 0, void 0)
  .ser(se_ImportConfigTomlCommand)
  .de(de_ImportConfigTomlCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: ImportConfigTomlInput;
      output: ImportConfigOutput;
  };
  sdk: {
      input: ImportConfigTomlCommandInput;
      output: ImportConfigTomlCommandOutput;
  };
};
}
