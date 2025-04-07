import { de_CreateExperimentCommand, se_CreateExperimentCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class CreateExperimentCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "CreateExperiment", {})
    .n("SuperpositionClient", "CreateExperimentCommand")
    .f(void 0, void 0)
    .ser(se_CreateExperimentCommand)
    .de(de_CreateExperimentCommand)
    .build() {
}
