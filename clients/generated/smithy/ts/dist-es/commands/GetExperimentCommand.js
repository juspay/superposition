import { de_GetExperimentCommand, se_GetExperimentCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class GetExperimentCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "GetExperiment", {})
    .n("SuperpositionClient", "GetExperimentCommand")
    .f(void 0, void 0)
    .ser(se_GetExperimentCommand)
    .de(de_GetExperimentCommand)
    .build() {
}
