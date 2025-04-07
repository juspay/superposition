import { de_UpdateDimensionCommand, se_UpdateDimensionCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class UpdateDimensionCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "UpdateDimension", {})
    .n("SuperpositionClient", "UpdateDimensionCommand")
    .f(void 0, void 0)
    .ser(se_UpdateDimensionCommand)
    .de(de_UpdateDimensionCommand)
    .build() {
}
