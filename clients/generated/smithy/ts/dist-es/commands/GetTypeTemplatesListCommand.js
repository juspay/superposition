import { de_GetTypeTemplatesListCommand, se_GetTypeTemplatesListCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class GetTypeTemplatesListCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "GetTypeTemplatesList", {})
    .n("SuperpositionClient", "GetTypeTemplatesListCommand")
    .f(void 0, void 0)
    .ser(se_GetTypeTemplatesListCommand)
    .de(de_GetTypeTemplatesListCommand)
    .build() {
}
