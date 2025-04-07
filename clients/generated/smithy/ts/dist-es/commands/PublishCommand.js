import { de_PublishCommand, se_PublishCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class PublishCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "Publish", {})
    .n("SuperpositionClient", "PublishCommand")
    .f(void 0, void 0)
    .ser(se_PublishCommand)
    .de(de_PublishCommand)
    .build() {
}
