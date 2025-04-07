import { de_UpdateFunctionCommand, se_UpdateFunctionCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class UpdateFunctionCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "UpdateFunction", {})
    .n("SuperpositionClient", "UpdateFunctionCommand")
    .f(void 0, void 0)
    .ser(se_UpdateFunctionCommand)
    .de(de_UpdateFunctionCommand)
    .build() {
}
