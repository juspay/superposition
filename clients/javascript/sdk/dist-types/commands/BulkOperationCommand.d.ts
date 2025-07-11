import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { BulkOperationInput, BulkOperationOutput } from "../models/models_0";
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
export interface BulkOperationCommandInput extends BulkOperationInput {
}
/**
 * @public
 *
 * The output of {@link BulkOperationCommand}.
 */
export interface BulkOperationCommandOutput extends BulkOperationOutput, __MetadataBearer {
}
declare const BulkOperationCommand_base: {
    new (input: BulkOperationCommandInput): import("@smithy/smithy-client").CommandImpl<BulkOperationCommandInput, BulkOperationCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: BulkOperationCommandInput): import("@smithy/smithy-client").CommandImpl<BulkOperationCommandInput, BulkOperationCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, BulkOperationCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, BulkOperationCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
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
 * //         PUT: { // ContextPutOut
 * //           context_id: "STRING_VALUE",
 * //           override_id: "STRING_VALUE",
 * //           weight: "STRING_VALUE",
 * //           description: "STRING_VALUE",
 * //           change_reason: "STRING_VALUE",
 * //         },
 * //         REPLACE: {
 * //           context_id: "STRING_VALUE",
 * //           override_id: "STRING_VALUE",
 * //           weight: "STRING_VALUE",
 * //           description: "STRING_VALUE",
 * //           change_reason: "STRING_VALUE",
 * //         },
 * //         DELETE: "STRING_VALUE",
 * //         MOVE: { // ContextMoveOut
 * //           context_id: "STRING_VALUE",
 * //           override_id: "STRING_VALUE",
 * //           weight: "STRING_VALUE",
 * //           description: "STRING_VALUE",
 * //           change_reason: "STRING_VALUE",
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
 */
export declare class BulkOperationCommand extends BulkOperationCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
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
