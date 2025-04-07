import { de_CreateWorkspaceCommand, se_CreateWorkspaceCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class CreateWorkspaceCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "CreateWorkspace", {})
    .n("SuperpositionClient", "CreateWorkspaceCommand")
    .f(void 0, void 0)
    .ser(se_CreateWorkspaceCommand)
    .de(de_CreateWorkspaceCommand)
    .build() {
}
