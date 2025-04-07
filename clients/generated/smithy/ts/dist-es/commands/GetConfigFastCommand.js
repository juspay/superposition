import { de_GetConfigFastCommand, se_GetConfigFastCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class GetConfigFastCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "GetConfigFast", {})
    .n("SuperpositionClient", "GetConfigFastCommand")
    .f(void 0, void 0)
    .ser(se_GetConfigFastCommand)
    .de(de_GetConfigFastCommand)
    .build() {
}
