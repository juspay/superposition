import { de_DiscardExperimentCommand, se_DiscardExperimentCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class DiscardExperimentCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "DiscardExperiment", {})
    .n("SuperpositionClient", "DiscardExperimentCommand")
    .f(void 0, void 0)
    .ser(se_DiscardExperimentCommand)
    .de(de_DiscardExperimentCommand)
    .build() {
}
