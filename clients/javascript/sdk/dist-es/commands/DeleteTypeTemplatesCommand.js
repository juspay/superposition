import { de_DeleteTypeTemplatesCommand, se_DeleteTypeTemplatesCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class DeleteTypeTemplatesCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "DeleteTypeTemplates", {})
    .n("SuperpositionClient", "DeleteTypeTemplatesCommand")
    .f(void 0, void 0)
    .ser(se_DeleteTypeTemplatesCommand)
    .de(de_DeleteTypeTemplatesCommand)
    .build() {
}
