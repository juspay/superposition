import { de_GetExperimentGroupCommand, se_GetExperimentGroupCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class GetExperimentGroupCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "GetExperimentGroup", {})
    .n("SuperpositionClient", "GetExperimentGroupCommand")
    .f(void 0, void 0)
    .ser(se_GetExperimentGroupCommand)
    .de(de_GetExperimentGroupCommand)
    .build() {
}
