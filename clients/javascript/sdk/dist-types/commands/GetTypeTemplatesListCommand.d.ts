import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { GetTypeTemplatesListInput, GetTypeTemplatesListOutput } from "../models/models_0";
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
 * The input for {@link GetTypeTemplatesListCommand}.
 */
export interface GetTypeTemplatesListCommandInput extends GetTypeTemplatesListInput {
}
/**
 * @public
 *
 * The output of {@link GetTypeTemplatesListCommand}.
 */
export interface GetTypeTemplatesListCommandOutput extends GetTypeTemplatesListOutput, __MetadataBearer {
}
declare const GetTypeTemplatesListCommand_base: {
    new (input: GetTypeTemplatesListCommandInput): import("@smithy/smithy-client").CommandImpl<GetTypeTemplatesListCommandInput, GetTypeTemplatesListCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: GetTypeTemplatesListCommandInput): import("@smithy/smithy-client").CommandImpl<GetTypeTemplatesListCommandInput, GetTypeTemplatesListCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetTypeTemplatesListCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetTypeTemplatesListCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetTypeTemplatesListInput
 *   count: Number("int"),
 *   page: Number("int"),
 *   all: true || false,
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 * };
 * const command = new GetTypeTemplatesListCommand(input);
 * const response = await client.send(command);
 * // { // GetTypeTemplatesListOutput
 * //   total_pages: Number("int"),
 * //   total_items: Number("int"),
 * //   data: [ // TypeTemplatesList
 * //     { // TypeTemplatesResponse
 * //       type_name: "STRING_VALUE", // required
 * //       type_schema: "DOCUMENT_VALUE", // required
 * //       description: "STRING_VALUE", // required
 * //       change_reason: "STRING_VALUE", // required
 * //       created_by: "STRING_VALUE", // required
 * //       created_at: new Date("TIMESTAMP"), // required
 * //       last_modified_at: new Date("TIMESTAMP"), // required
 * //       last_modified_by: "STRING_VALUE", // required
 * //     },
 * //   ],
 * // };
 *
 * ```
 *
 * @param GetTypeTemplatesListCommandInput - {@link GetTypeTemplatesListCommandInput}
 * @returns {@link GetTypeTemplatesListCommandOutput}
 * @see {@link GetTypeTemplatesListCommandInput} for command's `input` shape.
 * @see {@link GetTypeTemplatesListCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class GetTypeTemplatesListCommand extends GetTypeTemplatesListCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: GetTypeTemplatesListInput;
            output: GetTypeTemplatesListOutput;
        };
        sdk: {
            input: GetTypeTemplatesListCommandInput;
            output: GetTypeTemplatesListCommandOutput;
        };
    };
}
