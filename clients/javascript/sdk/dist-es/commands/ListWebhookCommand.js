import { de_ListWebhookCommand, se_ListWebhookCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class ListWebhookCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "ListWebhook", {})
    .n("SuperpositionClient", "ListWebhookCommand")
    .f(void 0, void 0)
    .ser(se_ListWebhookCommand)
    .de(de_ListWebhookCommand)
    .build() {
}
