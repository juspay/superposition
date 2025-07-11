import { de_RampExperimentCommand, se_RampExperimentCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class RampExperimentCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "RampExperiment", {})
    .n("SuperpositionClient", "RampExperimentCommand")
    .f(void 0, void 0)
    .ser(se_RampExperimentCommand)
    .de(de_RampExperimentCommand)
    .build() {
}
