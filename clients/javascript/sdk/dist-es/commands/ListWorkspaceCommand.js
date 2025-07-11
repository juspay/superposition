import { de_ListWorkspaceCommand, se_ListWorkspaceCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class ListWorkspaceCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "ListWorkspace", {})
    .n("SuperpositionClient", "ListWorkspaceCommand")
    .f(void 0, void 0)
    .ser(se_ListWorkspaceCommand)
    .de(de_ListWorkspaceCommand)
    .build() {
}
