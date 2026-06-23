// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ImportConfigJsonInput,
  ImportConfigOutput,
} from "../models/models_0";
import {
  de_ImportConfigJsonCommand,
  se_ImportConfigJsonCommand,
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
 * The input for {@link ImportConfigJsonCommand}.
 */
export interface ImportConfigJsonCommandInput extends ImportConfigJsonInput {}
/**
 * @public
 *
 * The output of {@link ImportConfigJsonCommand}.
 */
export interface ImportConfigJsonCommandOutput extends ImportConfigOutput, __MetadataBearer {}

/**
 * Imports a full config from a JSON document, persisting dimensions, default-configs and contexts in a single transaction after validating the document.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ImportConfigJsonCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ImportConfigJsonCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ImportConfigJsonInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   strategy: "create_only" || "upsert" || "replace",
 *   on_error: "abort" || "continue",
 *   dry_run: true || false,
 *   config_tags: "STRING_VALUE",
 *   json_config: "STRING_VALUE", // required
 * };
 * const command = new ImportConfigJsonCommand(input);
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
 * @param ImportConfigJsonCommandInput - {@link ImportConfigJsonCommandInput}
 * @returns {@link ImportConfigJsonCommandOutput}
 * @see {@link ImportConfigJsonCommandInput} for command's `input` shape.
 * @see {@link ImportConfigJsonCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class ImportConfigJsonCommand extends $Command.classBuilder<ImportConfigJsonCommandInput, ImportConfigJsonCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "ImportConfigJson", {

  })
  .n("SuperpositionClient", "ImportConfigJsonCommand")
  .f(void 0, void 0)
  .ser(se_ImportConfigJsonCommand)
  .de(de_ImportConfigJsonCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: ImportConfigJsonInput;
      output: ImportConfigOutput;
  };
  sdk: {
      input: ImportConfigJsonCommandInput;
      output: ImportConfigJsonCommandOutput;
  };
};
}
