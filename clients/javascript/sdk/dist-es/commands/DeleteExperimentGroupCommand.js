import { de_DeleteExperimentGroupCommand, se_DeleteExperimentGroupCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class DeleteExperimentGroupCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "DeleteExperimentGroup", {})
    .n("SuperpositionClient", "DeleteExperimentGroupCommand")
    .f(void 0, void 0)
    .ser(se_DeleteExperimentGroupCommand)
    .de(de_DeleteExperimentGroupCommand)
    .build() {
}
