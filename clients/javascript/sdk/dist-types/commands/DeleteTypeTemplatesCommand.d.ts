import { ServiceInputTypes, ServiceOutputTypes, SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { DeleteTypeTemplatesInput, TypeTemplatesResponse } from "../models/models_0";
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
 * The input for {@link DeleteTypeTemplatesCommand}.
 */
export interface DeleteTypeTemplatesCommandInput extends DeleteTypeTemplatesInput {
}
/**
 * @public
 *
 * The output of {@link DeleteTypeTemplatesCommand}.
 */
export interface DeleteTypeTemplatesCommandOutput extends TypeTemplatesResponse, __MetadataBearer {
}
declare const DeleteTypeTemplatesCommand_base: {
    new (input: DeleteTypeTemplatesCommandInput): import("@smithy/smithy-client").CommandImpl<DeleteTypeTemplatesCommandInput, DeleteTypeTemplatesCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    new (__0_0: DeleteTypeTemplatesCommandInput): import("@smithy/smithy-client").CommandImpl<DeleteTypeTemplatesCommandInput, DeleteTypeTemplatesCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>;
    getEndpointParameterInstructions(): import("@smithy/middleware-endpoint").EndpointParameterInstructions;
};
/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, DeleteTypeTemplatesCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, DeleteTypeTemplatesCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // DeleteTypeTemplatesInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   type_name: "STRING_VALUE", // required
 * };
 * const command = new DeleteTypeTemplatesCommand(input);
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
 * @param DeleteTypeTemplatesCommandInput - {@link DeleteTypeTemplatesCommandInput}
 * @returns {@link DeleteTypeTemplatesCommandOutput}
 * @see {@link DeleteTypeTemplatesCommandInput} for command's `input` shape.
 * @see {@link DeleteTypeTemplatesCommandOutput} for command's `response` shape.
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
export declare class DeleteTypeTemplatesCommand extends DeleteTypeTemplatesCommand_base {
    /** @internal type navigation helper, not in runtime. */
    protected static __types: {
        api: {
            input: DeleteTypeTemplatesInput;
            output: TypeTemplatesResponse;
        };
        sdk: {
            input: DeleteTypeTemplatesCommandInput;
            output: DeleteTypeTemplatesCommandOutput;
        };
    };
}
