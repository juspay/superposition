// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  BulkOperationInput,
  BulkOperationOutput,
} from "../models/models_0";
import {
  de_BulkOperationCommand,
  se_BulkOperationCommand,
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
 * The input for {@link BulkOperationCommand}.
 */
export interface BulkOperationCommandInput extends BulkOperationInput {}
/**
 * @public
 *
 * The output of {@link BulkOperationCommand}.
 */
export interface BulkOperationCommandOutput extends BulkOperationOutput, __MetadataBearer {}

/**
 * Executes multiple context operations (PUT, REPLACE, DELETE, MOVE) in a single atomic transaction for efficient batch processing.
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, BulkOperationCommand } from "superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, BulkOperationCommand } = require("superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // BulkOperationInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   config_tags: "STRING_VALUE",
 *   bulk_operation: { // BulkOperationReq
 *     operations: [ // BulkOperationList
 *       { // ContextAction Union: only one key present
 *         PUT: { // ContextPut
 *           context: { // Condition // required
 *             "<keys>": "DOCUMENT_VALUE",
 *           },
 *           override: { // Overrides // required
 *             "<keys>": "DOCUMENT_VALUE",
 *           },
 *           description: "STRING_VALUE",
 *           change_reason: "STRING_VALUE", // required
 *         },
 *         REPLACE: { // UpdateContextOverrideRequest
 *           context: { // ContextIdentifier Union: only one key present
 *             id: "STRING_VALUE",
 *             context: {
 *               "<keys>": "DOCUMENT_VALUE",
 *             },
 *           },
 *           override: { // required
 *             "<keys>": "DOCUMENT_VALUE",
 *           },
 *           description: "STRING_VALUE",
 *           change_reason: "STRING_VALUE", // required
 *         },
 *         DELETE: "STRING_VALUE",
 *         MOVE: { // ContextMove
 *           id: "STRING_VALUE",
 *           context: { // required
 *             "<keys>": "DOCUMENT_VALUE",
 *           },
 *           description: "STRING_VALUE",
 *           change_reason: "STRING_VALUE", // required
 *         },
 *       },
 *     ],
 *   },
 * };
 * const command = new BulkOperationCommand(input);
 * const response = await client.send(command);
 * // { // BulkOperationOutput
 * //   bulk_operation_output: { // BulkOperationOut
 * //     output: [ // BulkOperationOutList
 * //       { // ContextActionOut Union: only one key present
 * //         PUT: { // ContextResponse
 * //           id: "STRING_VALUE", // required
 * //           value: { // Condition
 * //             "<keys>": "DOCUMENT_VALUE",
 * //           },
 * //           override: { // Overrides
 * //             "<keys>": "DOCUMENT_VALUE",
 * //           },
 * //           override_id: "STRING_VALUE",
 * //           weight: "STRING_VALUE",
 * //           description: "STRING_VALUE",
 * //           change_reason: "STRING_VALUE",
 * //           created_at: new Date("TIMESTAMP"),
 * //           created_by: "STRING_VALUE",
 * //           last_modified_at: new Date("TIMESTAMP"),
 * //           last_modified_by: "STRING_VALUE",
 * //         },
 * //         REPLACE: {
 * //           id: "STRING_VALUE", // required
 * //           value: {
 * //             "<keys>": "DOCUMENT_VALUE",
 * //           },
 * //           override: {
 * //             "<keys>": "DOCUMENT_VALUE",
 * //           },
 * //           override_id: "STRING_VALUE",
 * //           weight: "STRING_VALUE",
 * //           description: "STRING_VALUE",
 * //           change_reason: "STRING_VALUE",
 * //           created_at: new Date("TIMESTAMP"),
 * //           created_by: "STRING_VALUE",
 * //           last_modified_at: new Date("TIMESTAMP"),
 * //           last_modified_by: "STRING_VALUE",
 * //         },
 * //         DELETE: "STRING_VALUE",
 * //         MOVE: {
 * //           id: "STRING_VALUE", // required
 * //           value: {
 * //             "<keys>": "DOCUMENT_VALUE",
 * //           },
 * //           override: {
 * //             "<keys>": "DOCUMENT_VALUE",
 * //           },
 * //           override_id: "STRING_VALUE",
 * //           weight: "STRING_VALUE",
 * //           description: "STRING_VALUE",
 * //           change_reason: "STRING_VALUE",
 * //           created_at: new Date("TIMESTAMP"),
 * //           created_by: "STRING_VALUE",
 * //           last_modified_at: new Date("TIMESTAMP"),
 * //           last_modified_by: "STRING_VALUE",
 * //         },
 * //       },
 * //     ],
 * //   },
 * // };
 *
 * ```
 *
 * @param BulkOperationCommandInput - {@link BulkOperationCommandInput}
 * @returns {@link BulkOperationCommandOutput}
 * @see {@link BulkOperationCommandInput} for command's `input` shape.
 * @see {@link BulkOperationCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 * @public
 */
export class BulkOperationCommand extends $Command.classBuilder<BulkOperationCommandInput, BulkOperationCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "BulkOperation", {

  })
  .n("SuperpositionClient", "BulkOperationCommand")
  .f(void 0, void 0)
  .ser(se_BulkOperationCommand)
  .de(de_BulkOperationCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: BulkOperationInput;
      output: BulkOperationOutput;
  };
  sdk: {
      input: BulkOperationCommandInput;
      output: BulkOperationCommandOutput;
  };
};
}
