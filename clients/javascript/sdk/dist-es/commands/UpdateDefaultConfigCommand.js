import { de_UpdateDefaultConfigCommand, se_UpdateDefaultConfigCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class UpdateDefaultConfigCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "UpdateDefaultConfig", {})
    .n("SuperpositionClient", "UpdateDefaultConfigCommand")
    .f(void 0, void 0)
    .ser(se_UpdateDefaultConfigCommand)
    .de(de_UpdateDefaultConfigCommand)
    .build() {
}
