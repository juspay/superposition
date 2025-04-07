import { de_GetResolvedConfigCommand, se_GetResolvedConfigCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class GetResolvedConfigCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "GetResolvedConfig", {})
    .n("SuperpositionClient", "GetResolvedConfigCommand")
    .f(void 0, void 0)
    .ser(se_GetResolvedConfigCommand)
    .de(de_GetResolvedConfigCommand)
    .build() {
}
