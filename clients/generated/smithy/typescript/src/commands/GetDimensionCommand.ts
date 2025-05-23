// smithy-typescript generated code
import {
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig,
} from "../SuperpositionClient";
import {
  DimensionExt,
  GetDimensionInput,
} from "../models/models_0";
import {
  de_GetDimensionCommand,
  se_GetDimensionCommand,
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
 * The input for {@link GetDimensionCommand}.
 */
export interface GetDimensionCommandInput extends GetDimensionInput {}
/**
 * @public
 *
 * The output of {@link GetDimensionCommand}.
 */
export interface GetDimensionCommandOutput extends DimensionExt, __MetadataBearer {}

/**
 * @public
 *
 * @example
 * Use a bare-bones client and the command you need to make an API call.
 * ```javascript
 * import { SuperpositionClient, GetDimensionCommand } from "io.juspay/superposition-typescript-sdk"; // ES Modules import
 * // const { SuperpositionClient, GetDimensionCommand } = require("io.juspay/superposition-typescript-sdk"); // CommonJS import
 * const client = new SuperpositionClient(config);
 * const input = { // GetDimensionInput
 *   workspace_id: "STRING_VALUE", // required
 *   org_id: "STRING_VALUE", // required
 *   dimension: "STRING_VALUE", // required
 * };
 * const command = new GetDimensionCommand(input);
 * const response = await client.send(command);
 * // { // DimensionExt
 * //   dimension: "STRING_VALUE", // required
 * //   position: Number("int"), // required
 * //   schema: "DOCUMENT_VALUE", // required
 * //   function_name: "STRING_VALUE",
 * //   description: "STRING_VALUE", // required
 * //   change_reason: "STRING_VALUE", // required
 * //   last_modified_at: new Date("TIMESTAMP"), // required
 * //   last_modified_by: "STRING_VALUE", // required
 * //   created_at: new Date("TIMESTAMP"), // required
 * //   created_by: "STRING_VALUE", // required
 * //   dependencies: [ // Dependencies // required
 * //     "STRING_VALUE",
 * //   ],
 * //   dependents: [ // Dependents // required
 * //     "STRING_VALUE",
 * //   ],
 * //   dependency_graph: { // Object // required
 * //     "<keys>": "DOCUMENT_VALUE",
 * //   },
 * //   mandatory: true || false,
 * // };
 *
 * ```
 *
 * @param GetDimensionCommandInput - {@link GetDimensionCommandInput}
 * @returns {@link GetDimensionCommandOutput}
 * @see {@link GetDimensionCommandInput} for command's `input` shape.
 * @see {@link GetDimensionCommandOutput} for command's `response` shape.
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
export class GetDimensionCommand extends $Command.classBuilder<GetDimensionCommandInput, GetDimensionCommandOutput, SuperpositionClientResolvedConfig, ServiceInputTypes, ServiceOutputTypes>()
      .m(function (this: any, Command: any, cs: any, config: SuperpositionClientResolvedConfig, o: any) {
          return [

  getSerdePlugin(config, this.serialize, this.deserialize),
      ];
  })
  .s("Superposition", "GetDimension", {

  })
  .n("SuperpositionClient", "GetDimensionCommand")
  .f(void 0, void 0)
  .ser(se_GetDimensionCommand)
  .de(de_GetDimensionCommand)
.build() {
/** @internal type navigation helper, not in runtime. */
declare protected static __types: {
  api: {
      input: GetDimensionInput;
      output: DimensionExt;
  };
  sdk: {
      input: GetDimensionCommandInput;
      output: GetDimensionCommandOutput;
  };
};
}
