import { de_CreateContextCommand, se_CreateContextCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class CreateContextCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "CreateContext", {})
    .n("SuperpositionClient", "CreateContextCommand")
    .f(void 0, void 0)
    .ser(se_CreateContextCommand)
    .de(de_CreateContextCommand)
    .build() {
}
