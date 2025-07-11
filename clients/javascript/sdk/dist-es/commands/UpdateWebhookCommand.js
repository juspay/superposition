import { de_UpdateWebhookCommand, se_UpdateWebhookCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class UpdateWebhookCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "UpdateWebhook", {})
    .n("SuperpositionClient", "UpdateWebhookCommand")
    .f(void 0, void 0)
    .ser(se_UpdateWebhookCommand)
    .de(de_UpdateWebhookCommand)
    .build() {
}
