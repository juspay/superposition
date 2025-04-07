import { de_CreateDimensionCommand, se_CreateDimensionCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class CreateDimensionCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "CreateDimension", {})
    .n("SuperpositionClient", "CreateDimensionCommand")
    .f(void 0, void 0)
    .ser(se_CreateDimensionCommand)
    .de(de_CreateDimensionCommand)
    .build() {
}
