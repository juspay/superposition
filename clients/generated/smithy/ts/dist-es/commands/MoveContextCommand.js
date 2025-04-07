import { de_MoveContextCommand, se_MoveContextCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class MoveContextCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "MoveContext", {})
    .n("SuperpositionClient", "MoveContextCommand")
    .f(void 0, void 0)
    .ser(se_MoveContextCommand)
    .de(de_MoveContextCommand)
    .build() {
}
