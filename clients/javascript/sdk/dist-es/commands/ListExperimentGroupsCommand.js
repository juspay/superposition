import { de_ListExperimentGroupsCommand, se_ListExperimentGroupsCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class ListExperimentGroupsCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "ListExperimentGroups", {})
    .n("SuperpositionClient", "ListExperimentGroupsCommand")
    .f(void 0, void 0)
    .ser(se_ListExperimentGroupsCommand)
    .de(de_ListExperimentGroupsCommand)
    .build() {
}
