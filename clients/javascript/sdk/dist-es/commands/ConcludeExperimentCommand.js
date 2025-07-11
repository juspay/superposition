import { de_ConcludeExperimentCommand, se_ConcludeExperimentCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class ConcludeExperimentCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "ConcludeExperiment", {})
    .n("SuperpositionClient", "ConcludeExperimentCommand")
    .f(void 0, void 0)
    .ser(se_ConcludeExperimentCommand)
    .de(de_ConcludeExperimentCommand)
    .build() {
}
