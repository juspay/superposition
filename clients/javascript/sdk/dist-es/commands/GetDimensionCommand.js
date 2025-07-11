import { de_GetDimensionCommand, se_GetDimensionCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class GetDimensionCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "GetDimension", {})
    .n("SuperpositionClient", "GetDimensionCommand")
    .f(void 0, void 0)
    .ser(se_GetDimensionCommand)
    .de(de_GetDimensionCommand)
    .build() {
}
