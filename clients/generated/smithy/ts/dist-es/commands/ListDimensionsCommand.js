import { de_ListDimensionsCommand, se_ListDimensionsCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class ListDimensionsCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "ListDimensions", {})
    .n("SuperpositionClient", "ListDimensionsCommand")
    .f(void 0, void 0)
    .ser(se_ListDimensionsCommand)
    .de(de_ListDimensionsCommand)
    .build() {
}
