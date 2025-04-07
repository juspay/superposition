import { de_UpdateOverrideCommand, se_UpdateOverrideCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class UpdateOverrideCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "UpdateOverride", {})
    .n("SuperpositionClient", "UpdateOverrideCommand")
    .f(void 0, void 0)
    .ser(se_UpdateOverrideCommand)
    .de(de_UpdateOverrideCommand)
    .build() {
}
