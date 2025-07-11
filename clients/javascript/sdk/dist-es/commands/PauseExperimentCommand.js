import { de_PauseExperimentCommand, se_PauseExperimentCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class PauseExperimentCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "PauseExperiment", {})
    .n("SuperpositionClient", "PauseExperimentCommand")
    .f(void 0, void 0)
    .ser(se_PauseExperimentCommand)
    .de(de_PauseExperimentCommand)
    .build() {
}
