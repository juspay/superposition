import { de_AddMembersToGroupCommand, se_AddMembersToGroupCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class AddMembersToGroupCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "AddMembersToGroup", {})
    .n("SuperpositionClient", "AddMembersToGroupCommand")
    .f(void 0, void 0)
    .ser(se_AddMembersToGroupCommand)
    .de(de_AddMembersToGroupCommand)
    .build() {
}
