import { de_ListOrganisationCommand, se_ListOrganisationCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class ListOrganisationCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "ListOrganisation", {})
    .n("SuperpositionClient", "ListOrganisationCommand")
    .f(void 0, void 0)
    .ser(se_ListOrganisationCommand)
    .de(de_ListOrganisationCommand)
    .build() {
}
