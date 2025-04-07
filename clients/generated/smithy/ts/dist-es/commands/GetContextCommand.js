import { de_GetContextCommand, se_GetContextCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class GetContextCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "GetContext", {})
    .n("SuperpositionClient", "GetContextCommand")
    .f(void 0, void 0)
    .ser(se_GetContextCommand)
    .de(de_GetContextCommand)
    .build() {
}
