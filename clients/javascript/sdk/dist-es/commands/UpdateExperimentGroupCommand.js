import { de_UpdateExperimentGroupCommand, se_UpdateExperimentGroupCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class UpdateExperimentGroupCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "UpdateExperimentGroup", {})
    .n("SuperpositionClient", "UpdateExperimentGroupCommand")
    .f(void 0, void 0)
    .ser(se_UpdateExperimentGroupCommand)
    .de(de_UpdateExperimentGroupCommand)
    .build() {
}
