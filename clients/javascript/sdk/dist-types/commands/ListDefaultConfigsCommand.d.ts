import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { ListDefaultConfigsInput, ListDefaultConfigsOutput } from "../models/models_0";
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
 * The input for {@link ListDefaultConfigsCommand}.
 */
export interface ListDefaultConfigsCommandInput extends ListDefaultConfigsInput {
}
/**
 * @public
 *
 * The output of {@link ListDefaultConfigsCommand}.
 */
export interface ListDefaultConfigsCommandOutput extends ListDefaultConfigsOutput, __MetadataBearer {
}
declare const ListDefaultConfigsCommand_base: {
    new (input: ListDefaultConfigsCommandInput): import("@smithy/smithy-client").CommandImpl<ListDefaultConfigsCommandInput, ListDefaultConfigsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: ListDefaultConfigsCommandInput): import("@smithy/smithy-client").CommandImpl<ListDefaultConfigsCommandInput, ListDefaultConfigsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListDefaultConfigsCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListDefaultConfigsCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListDefaultConfigsInput
 *   count: Number("int"),
 *   page: Number("int"),
 *   all: true || false,
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 * };
 * const command = new ListDefaultConfigsCommand(input);
 * const response = await client.send(command);
 * // { // ListDefaultConfigsOutput
 * //   total_pages: Number("int"),
 * //   total_items: Number("int"),
 * //   data: [ // ListDefaultConfigOut
 * //     { // DefaultConfigFull
 * //       key: "STRING_VALUE", // required
 * //       value: "DOCUMENT_VALUE", // required
 * //       schema: "DOCUMENT_VALUE", // required
 * //       description: "STRING_VALUE", // required
 * //       change_reason: "STRING_VALUE", // required
 * //       function_name: "STRING_VALUE",
 * //       autocomplete_function_name: "STRING_VALUE",
 * //       created_at: new Date("TIMESTAMP"), // required
 * //       created_by: "STRING_VALUE", // required
 * //       last_modified_at: new Date("TIMESTAMP"), // required
 * //       last_modified_by: "STRING_VALUE", // required
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param ListDefaultConfigsCommandInput - {@link ListDefaultConfigsCommandInput}
 * @returns {@link ListDefaultConfigsCommandOutput}
 * @see {@link ListDefaultConfigsCommandInput} for command's `input` shape.
 * @see {@link ListDefaultConfigsCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link ResourceNotFound} (client fault)
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class ListDefaultConfigsCommand extends ListDefaultConfigsCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: ListDefaultConfigsInput;
            output: ListDefaultConfigsOutput;
        };
        sdk: {
            input: ListDefaultConfigsCommandInput;
            output: ListDefaultConfigsCommandOutput;
        };
    };
}
