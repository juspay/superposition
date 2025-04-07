import { de_TestCommand, se_TestCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class TestCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "Test", {})
    .n("SuperpositionClient", "TestCommand")
    .f(void 0, void 0)
    .ser(se_TestCommand)
    .de(de_TestCommand)
    .build() {
}
