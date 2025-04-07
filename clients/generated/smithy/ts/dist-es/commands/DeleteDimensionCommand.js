import { de_DeleteDimensionCommand, se_DeleteDimensionCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class DeleteDimensionCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "DeleteDimension", {})
    .n("SuperpositionClient", "DeleteDimensionCommand")
    .f(void 0, void 0)
    .ser(se_DeleteDimensionCommand)
    .de(de_DeleteDimensionCommand)
    .build() {
}
