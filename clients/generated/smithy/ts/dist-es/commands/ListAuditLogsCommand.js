import { de_ListAuditLogsCommand, se_ListAuditLogsCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class ListAuditLogsCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "ListAuditLogs", {})
    .n("SuperpositionClient", "ListAuditLogsCommand")
    .f(void 0, void 0)
    .ser(se_ListAuditLogsCommand)
    .de(de_ListAuditLogsCommand)
    .build() {
}
