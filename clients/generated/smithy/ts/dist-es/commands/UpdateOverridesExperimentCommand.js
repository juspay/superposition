import { de_UpdateOverridesExperimentCommand, se_UpdateOverridesExperimentCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class UpdateOverridesExperimentCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "UpdateOverridesExperiment", {})
    .n("SuperpositionClient", "UpdateOverridesExperimentCommand")
    .f(void 0, void 0)
    .ser(se_UpdateOverridesExperimentCommand)
    .de(de_UpdateOverridesExperimentCommand)
    .build() {
}
