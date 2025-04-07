import { de_ListFunctionCommand, se_ListFunctionCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class ListFunctionCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "ListFunction", {})
    .n("SuperpositionClient", "ListFunctionCommand")
    .f(void 0, void 0)
    .ser(se_ListFunctionCommand)
    .de(de_ListFunctionCommand)
    .build() {
}
