import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { TypeTemplatesResponse, UpdateTypeTemplatesRequest } from "../models/models_0";
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
 * The input for {@link UpdateTypeTemplatesCommand}.
 */
export interface UpdateTypeTemplatesCommandInput extends UpdateTypeTemplatesRequest {
}
/**
 * @public
 *
 * The output of {@link UpdateTypeTemplatesCommand}.
 */
export interface UpdateTypeTemplatesCommandOutput extends TypeTemplatesResponse, __MetadataBearer {
}
declare const UpdateTypeTemplatesCommand_base: {
    new (input: UpdateTypeTemplatesCommandInput): import("@smithy/smithy-client").CommandImpl<UpdateTypeTemplatesCommandInput, UpdateTypeTemplatesCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: UpdateTypeTemplatesCommandInput): import("@smithy/smithy-client").CommandImpl<UpdateTypeTemplatesCommandInput, UpdateTypeTemplatesCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, UpdateTypeTemplatesCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, UpdateTypeTemplatesCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // UpdateTypeTemplatesRequest
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   type_name: "STRING_VALUE", // required
 *   type_schema: "DOCUMENT_VALUE", // required
 *   description: "STRING_VALUE",
 *   change_reason: "STRING_VALUE", // required
 * };
 * const command = new UpdateTypeTemplatesCommand(input);
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
 * @param UpdateTypeTemplatesCommandInput - {@link UpdateTypeTemplatesCommandInput}
 * @returns {@link UpdateTypeTemplatesCommandOutput}
 * @see {@link UpdateTypeTemplatesCommandInput} for command's `input` shape.
 * @see {@link UpdateTypeTemplatesCommandOutput} for command's `response` shape.
 * @see {@link SuperpositionClientResolvedConfig | config} for SuperpositionClient's `config` shape.
 *
 * @throws {@link TypeTemplatesNotFound} (client fault)
 *
 * @throws {@link InternalServerError} (server fault)
 *
 * @throws {@link SuperpositionServiceException}
 * <p>Base exception class for all service exceptions from Superposition service.</p>
 *
 */
export declare class UpdateTypeTemplatesCommand extends UpdateTypeTemplatesCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: UpdateTypeTemplatesRequest;
            output: TypeTemplatesResponse;
        };
        sdk: {
            input: UpdateTypeTemplatesCommandInput;
            output: UpdateTypeTemplatesCommandOutput;
        };
    };
}
