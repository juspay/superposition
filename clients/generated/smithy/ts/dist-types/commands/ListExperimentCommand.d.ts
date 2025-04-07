import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { ExperimentListResponse, ListExperimentInput } from "../models/models_0";
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
 * The input for {@link ListExperimentCommand}.
 */
export interface ListExperimentCommandInput extends ListExperimentInput {
}
/**
 * @public
 *
 * The output of {@link ListExperimentCommand}.
 */
export interface ListExperimentCommandOutput extends ExperimentListResponse, __MetadataBearer {
}
declare const ListExperimentCommand_base: {
    new (input: ListExperimentCommandInput): import("@smithy/smithy-client").CommandImpl<ListExperimentCommandInput, ListExperimentCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: ListExperimentCommandInput): import("@smithy/smithy-client").CommandImpl<ListExperimentCommandInput, ListExperimentCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, ListExperimentCommand } from "@io.juspay/superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, ListExperimentCommand } = require("@io.juspay/superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // ListExperimentInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   page: Number("long"),
 *   count: Number("long"),
 *   all: true || false,
 *   status: "CREATED" || "CONCLUDED" || "INPROGRESS" || "DISCARDED",
 *   from_date: new Date("TIMESTAMP"),
 *   to_date: new Date("TIMESTAMP"),
 *   experiment_name: "STRING_VALUE",
 *   experiment_ids: "STRING_VALUE",
 *   created_by: "STRING_VALUE",
 *   context_query: "STRING_VALUE",
 *   sort_on: "last_modified_at" || "created_at",
 *   sort_by: "desc" || "asc",
 * };
 * const command = new ListExperimentCommand(input);
 * const response = await client.send(command);
 * // { // ExperimentListResponse
 * //   total_pages: Number("long"), // required
 * //   total_items: Number("long"), // required
 * //   data: [ // ExperimentList // required
 * //     { // ExperimentResponse
 * //       id: "STRING_VALUE", // required
 * //       created_at: new Date("TIMESTAMP"), // required
 * //       created_by: "STRING_VALUE", // required
 * //       last_modified: new Date("TIMESTAMP"), // required
 * //       name: "STRING_VALUE", // required
 * //       override_keys: [ // ListOverrideKeys // required
 * //         "STRING_VALUE",
 * //       ],
 * //       status: "CREATED" || "CONCLUDED" || "INPROGRESS" || "DISCARDED", // required
 * //       traffic_percentage: Number("int"), // required
 * //       context: { // Condition // required
 * //         "<keys>": "DOCUMENT_VALUE",
 * //       },
 * //       variants: [ // ListVariant // required
 * //         { // Variant
 * //           id: "STRING_VALUE", // required
 * //           variant_type: "CONTROL" || "EXPERIMENTAL", // required
 * //           context_id: "STRING_VALUE",
 * //           override_id: "STRING_VALUE",
 * //           overrides: "DOCUMENT_VALUE", // required
 * //         },
 * //       ],
 * //       last_modified_by: "STRING_VALUE", // required
 * //       chosen_variant: "STRING_VALUE",
 * //       description: "STRING_VALUE", // required
 * //       change_reason: "STRING_VALUE", // required
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param ListExperimentCommandInput - {@link ListExperimentCommandInput}
 * @returns {@link ListExperimentCommandOutput}
 * @see {@link ListExperimentCommandInput} for command's `input` shape.
 * @see {@link ListExperimentCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class ListExperimentCommand extends ListExperimentCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: ListExperimentInput;
            output: ExperimentListResponse;
        };
        sdk: {
            input: ListExperimentCommandInput;
            output: ListExperimentCommandOutput;
        };
    };
}
