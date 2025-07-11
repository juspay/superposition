import { de_CreateTypeTemplatesCommand, se_CreateTypeTemplatesCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class CreateTypeTemplatesCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "CreateTypeTemplates", {})
    .n("SuperpositionClient", "CreateTypeTemplatesCommand")
    .f(void 0, void 0)
    .ser(se_CreateTypeTemplatesCommand)
    .de(de_CreateTypeTemplatesCommand)
    .build() {
}
