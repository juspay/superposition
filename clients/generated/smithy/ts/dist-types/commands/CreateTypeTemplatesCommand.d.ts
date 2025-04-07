import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { CreateTypeTemplatesRequest, TypeTemplatesResponse } from "../models/models_0";
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
 * The input for {@link CreateTypeTemplatesCommand}.
 */
export interface CreateTypeTemplatesCommandInput extends CreateTypeTemplatesRequest {
}
/**
 * @public
 *
 * The output of {@link CreateTypeTemplatesCommand}.
 */
export interface CreateTypeTemplatesCommandOutput extends TypeTemplatesResponse, __MetadataBearer {
}
declare const CreateTypeTemplatesCommand_base: {
    new (input: CreateTypeTemplatesCommandInput): import("@smithy/smithy-client").CommandImpl<CreateTypeTemplatesCommandInput, CreateTypeTemplatesCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: CreateTypeTemplatesCommandInput): import("@smithy/smithy-client").CommandImpl<CreateTypeTemplatesCommandInput, CreateTypeTemplatesCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, CreateTypeTemplatesCommand } from "@io.juspay/superposition-sdk"; // ES Modules import
 * // const { SuperpositionClient, CreateTypeTemplatesCommand } = require("@io.juspay/superposition-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // CreateTypeTemplatesRequest
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   type_name: "STRING_VALUE", // required
 *   type_schema: "DOCUMENT_VALUE", // required
 *   description: "STRING_VALUE", // required
 *   change_reason: "STRING_VALUE", // required
 * };
 * const command = new CreateTypeTemplatesCommand(input);
 * const response = await client.send(command);
 * // { // TypeTemplatesResponse
 * //   type_name: "STRING_VALUE", // required
 * //   type_schema: "DOCUMENT_VALUE", // required
 * //   description: "STRING_VALUE", // required
 * //   change_reason: "STRING_VALUE", // required
 * //   created_by: "STRING_VALUE", // required
 * //   created_at: new Date("TIMESTAMP"), // required
 * //   last_modified_at: new Date("TIMESTAMP"), // required
 * //   last_modified_by: "STRING_VALUE", // required
 * // };
 *
 * ```
 *
 * @param CreateTypeTemplatesCommandInput - {@link CreateTypeTemplatesCommandInput}
 * @returns {@link CreateTypeTemplatesCommandOutput}
 * @see {@link CreateTypeTemplatesCommandInput} for command's `input` shape.
 * @see {@link CreateTypeTemplatesCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class CreateTypeTemplatesCommand extends CreateTypeTemplatesCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: CreateTypeTemplatesRequest;
            output: TypeTemplatesResponse;
        };
        sdk: {
            input: CreateTypeTemplatesCommandInput;
            output: CreateTypeTemplatesCommandOutput;
        };
    };
}
