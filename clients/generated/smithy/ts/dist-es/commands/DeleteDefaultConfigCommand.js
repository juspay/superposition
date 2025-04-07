import { de_DeleteDefaultConfigCommand, se_DeleteDefaultConfigCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class DeleteDefaultConfigCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "DeleteDefaultConfig", {})
    .n("SuperpositionClient", "DeleteDefaultConfigCommand")
    .f(void 0, void 0)
    .ser(se_DeleteDefaultConfigCommand)
    .de(de_DeleteDefaultConfigCommand)
    .build() {
}
