import { de_ListExperimentCommand, se_ListExperimentCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class ListExperimentCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "ListExperiment", {})
    .n("SuperpositionClient", "ListExperimentCommand")
    .f(void 0, void 0)
    .ser(se_ListExperimentCommand)
    .de(de_ListExperimentCommand)
    .build() {
}
