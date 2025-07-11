import { de_GetWebhookCommand, se_GetWebhookCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class GetWebhookCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "GetWebhook", {})
    .n("SuperpositionClient", "GetWebhookCommand")
    .f(void 0, void 0)
    .ser(se_GetWebhookCommand)
    .de(de_GetWebhookCommand)
    .build() {
}
