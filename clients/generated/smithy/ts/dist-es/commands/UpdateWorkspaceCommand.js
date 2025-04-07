import { de_UpdateWorkspaceCommand, se_UpdateWorkspaceCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class UpdateWorkspaceCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "UpdateWorkspace", {})
    .n("SuperpositionClient", "UpdateWorkspaceCommand")
    .f(void 0, void 0)
    .ser(se_UpdateWorkspaceCommand)
    .de(de_UpdateWorkspaceCommand)
    .build() {
}
