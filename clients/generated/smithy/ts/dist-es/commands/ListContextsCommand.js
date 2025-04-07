import { de_ListContextsCommand, se_ListContextsCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class ListContextsCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "ListContexts", {})
    .n("SuperpositionClient", "ListContextsCommand")
    .f(void 0, void 0)
    .ser(se_ListContextsCommand)
    .de(de_ListContextsCommand)
    .build() {
}
