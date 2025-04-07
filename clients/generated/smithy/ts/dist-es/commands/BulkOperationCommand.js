import { de_BulkOperationCommand, se_BulkOperationCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class BulkOperationCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "BulkOperation", {})
    .n("SuperpositionClient", "BulkOperationCommand")
    .f(void 0, void 0)
    .ser(se_BulkOperationCommand)
    .de(de_BulkOperationCommand)
    .build() {
}
