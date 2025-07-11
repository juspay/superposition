import { de_ApplicableVariantsCommand, se_ApplicableVariantsCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class ApplicableVariantsCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "ApplicableVariants", {})
    .n("SuperpositionClient", "ApplicableVariantsCommand")
    .f(void 0, void 0)
    .ser(se_ApplicableVariantsCommand)
    .de(de_ApplicableVariantsCommand)
    .build() {
}
