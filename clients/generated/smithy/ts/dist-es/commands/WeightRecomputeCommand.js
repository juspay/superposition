import { de_WeightRecomputeCommand, se_WeightRecomputeCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class WeightRecomputeCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "WeightRecompute", {})
    .n("SuperpositionClient", "WeightRecomputeCommand")
    .f(void 0, void 0)
    .ser(se_WeightRecomputeCommand)
    .de(de_WeightRecomputeCommand)
    .build() {
}
