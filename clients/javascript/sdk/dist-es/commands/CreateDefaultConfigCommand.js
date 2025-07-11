import { de_CreateDefaultConfigCommand, se_CreateDefaultConfigCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class CreateDefaultConfigCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "CreateDefaultConfig", {})
    .n("SuperpositionClient", "CreateDefaultConfigCommand")
    .f(void 0, void 0)
    .ser(se_CreateDefaultConfigCommand)
    .de(de_CreateDefaultConfigCommand)
    .build() {
}
