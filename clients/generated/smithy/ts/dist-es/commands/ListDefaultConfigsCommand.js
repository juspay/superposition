import { de_ListDefaultConfigsCommand, se_ListDefaultConfigsCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class ListDefaultConfigsCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "ListDefaultConfigs", {})
    .n("SuperpositionClient", "ListDefaultConfigsCommand")
    .f(void 0, void 0)
    .ser(se_ListDefaultConfigsCommand)
    .de(de_ListDefaultConfigsCommand)
    .build() {
}
