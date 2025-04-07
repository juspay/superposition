import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { ListContextsInput, ListContextsOutput } from "../models/models_0";
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
 * The input for {@link ListContextsCommand}.
 */
export interface ListContextsCommandInput extends ListContextsInput {
}
/**
 * @public
 *
 * The output of {@link ListContextsCommand}.
 */
export interface ListContextsCommandOutput extends ListContextsOutput, __MetadataBearer {
}
declare const ListContextsCommand_base: {
    new (input: ListContextsCommandInput): import("@smithy/smithy-client").CommandImpl<ListContextsCommandInput, ListContextsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: ListContextsCommandInput): import("@smithy/smithy-client").CommandImpl<ListContextsCommandInput, ListContextsCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListContextsCommand } from "@io.juspay/superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListContextsCommand } = require("@io.juspay/superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListContextsInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   page: Number("int"),
 *   count: Number("int"),
 *   prefix: "STRING_VALUE",
 *   sort_on: "last_modified_at" || "created_at" || "weight",
 *   sort_by: "desc" || "asc",
 *   created_by: "STRING_VALUE",
 *   last_modified_by: "STRING_VALUE",
 *   plaintext: "STRING_VALUE",
 * };
 * const command = new ListContextsCommand(input);
 * const response = await client.send(command);
 * // { // ListContextsOutput
 * //   total_pages: Number("int"),
 * //   total_items: Number("int"),
 * //   data: [ // ListContextOut
 * //     { // ContextFull
 * //       id: "STRING_VALUE", // required
 * //       value: { // Condition
 * //         "<keys>": "DOCUMENT_VALUE",
 * //       },
 * //       override: { // Overrides
 * //         "<keys>": "DOCUMENT_VALUE",
 * //       },
 * //       override_id: "STRING_VALUE",
 * //       weight: "STRING_VALUE",
 * //       description: "STRING_VALUE",
 * //       change_reason: "STRING_VALUE",
 * //       created_at: new Date("TIMESTAMP"),
 * //       created_by: "STRING_VALUE",
 * //       last_modified_at: new Date("TIMESTAMP"),
 * //       last_modified_by: "STRING_VALUE",
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param ListContextsCommandInput - {@link ListContextsCommandInput}
 * @returns {@link ListContextsCommandOutput}
 * @see {@link ListContextsCommandInput} for command's `input` shape.
 * @see {@link ListContextsCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class ListContextsCommand extends ListContextsCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: ListContextsInput;
            output: ListContextsOutput;
        };
        sdk: {
            input: ListContextsCommandInput;
            output: ListContextsCommandOutput;
        };
    };
}
