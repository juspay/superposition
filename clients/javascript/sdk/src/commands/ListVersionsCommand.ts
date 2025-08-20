// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  ListVersionsInput,
  ListVersionsOutput,
} from "../models/models_0";
import {
  de_ListVersionsCommand,
  se_ListVersionsCommand,
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
 * The input for {@link ListVersionsCommand}.
 */
export interface ListVersionsCommandInput extends ListVersionsInput {}
/**
 * @public
 *
 * The output of {@link ListVersionsCommand}.
 */
export interface ListVersionsCommandOutput extends ListVersionsOutput, __MetadataBearer {}

/**
 * Retrieves a paginated list of config versions with their metadata, hash values, and creation timestamps for audit and rollback purposes.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListVersionsCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListVersionsCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListVersionsInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   count: Number("int"),
 *   page: Number("int"),
 * };
 * const command = new ListVersionsCommand(input);
 * const response = await client.send(command);
 * // { // ListVersionsOutput
 * //   total_pages: Number("int"), // required
 * //   total_items: Number("int"), // required
 * //   data: [ // ListVersionsOut // required
 * //     { // ListVersionsMember
 * //       id: "STRING_VALUE", // required
 * //       config: "DOCUMENT_VALUE", // required
 * //       config_hash: "STRING_VALUE", // required
 * //       created_at: new Date("TIMESTAMP"), // required
 * //       description: "STRING_VALUE", // required
 * //       tags: [ // StringList
 * //         "STRING_VALUE",
 * //       ],
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param ListVersionsCommandInput - {@link ListVersionsCommandInput}
 * @returns {@link ListVersionsCommandOutput}
 * @see {@link ListVersionsCommandInput} for command's `input` shape.
 * @see {@link ListVersionsCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class ListVersionsCommand extends $Command.classBuilder<ListVersionsCommandInput, ListVersionsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "ListVersions", {

  })
  .n("SuperpositionClient", "ListVersionsCommand")
  .f(void 0, void 0)
  .ser(se_ListVersionsCommand)
  .de(de_ListVersionsCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: ListVersionsInput;
      output: ListVersionsOutput;
  };
  sdk: {
      input: ListVersionsCommandInput;
      output: ListVersionsCommandOutput;
  };
};
}
