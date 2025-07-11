import { de_ResumeExperimentCommand, se_ResumeExperimentCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class ResumeExperimentCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "ResumeExperiment", {})
    .n("SuperpositionClient", "ResumeExperimentCommand")
    .f(void 0, void 0)
    .ser(se_ResumeExperimentCommand)
    .de(de_ResumeExperimentCommand)
    .build() {
}
