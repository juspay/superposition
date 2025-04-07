import { de_CreateFunctionCommand, se_CreateFunctionCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class CreateFunctionCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "CreateFunction", {})
    .n("SuperpositionClient", "CreateFunctionCommand")
    .f(void 0, void 0)
    .ser(se_CreateFunctionCommand)
    .de(de_CreateFunctionCommand)
    .build() {
}
