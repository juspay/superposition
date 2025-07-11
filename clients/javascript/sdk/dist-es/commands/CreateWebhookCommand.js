import { de_CreateWebhookCommand, se_CreateWebhookCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class CreateWebhookCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "CreateWebhook", {})
    .n("SuperpositionClient", "CreateWebhookCommand")
    .f(void 0, void 0)
    .ser(se_CreateWebhookCommand)
    .de(de_CreateWebhookCommand)
    .build() {
}
