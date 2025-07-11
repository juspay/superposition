import { de_UpdateTypeTemplatesCommand, se_UpdateTypeTemplatesCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class UpdateTypeTemplatesCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "UpdateTypeTemplates", {})
    .n("SuperpositionClient", "UpdateTypeTemplatesCommand")
    .f(void 0, void 0)
    .ser(se_UpdateTypeTemplatesCommand)
    .de(de_UpdateTypeTemplatesCommand)
    .build() {
}
