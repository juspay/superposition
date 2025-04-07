import { de_DeleteContextCommand, se_DeleteContextCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class DeleteContextCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "DeleteContext", {})
    .n("SuperpositionClient", "DeleteContextCommand")
    .f(void 0, void 0)
    .ser(se_DeleteContextCommand)
    .de(de_DeleteContextCommand)
    .build() {
}
