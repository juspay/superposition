import { de_DeleteFunctionCommand, se_DeleteFunctionCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class DeleteFunctionCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "DeleteFunction", {})
    .n("SuperpositionClient", "DeleteFunctionCommand")
    .f(void 0, void 0)
    .ser(se_DeleteFunctionCommand)
    .de(de_DeleteFunctionCommand)
    .build() {
}
