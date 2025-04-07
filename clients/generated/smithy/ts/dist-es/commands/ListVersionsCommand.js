import { de_ListVersionsCommand, se_ListVersionsCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class ListVersionsCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "ListVersions", {})
    .n("SuperpositionClient", "ListVersionsCommand")
    .f(void 0, void 0)
    .ser(se_ListVersionsCommand)
    .de(de_ListVersionsCommand)
    .build() {
}
