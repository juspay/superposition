import { de_RemoveMembersFromGroupCommand, se_RemoveMembersFromGroupCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class RemoveMembersFromGroupCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "RemoveMembersFromGroup", {})
    .n("SuperpositionClient", "RemoveMembersFromGroupCommand")
    .f(void 0, void 0)
    .ser(se_RemoveMembersFromGroupCommand)
    .de(de_RemoveMembersFromGroupCommand)
    .build() {
}
