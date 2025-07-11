import { de_UpdateOrganisationCommand, se_UpdateOrganisationCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class UpdateOrganisationCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "UpdateOrganisation", {})
    .n("SuperpositionClient", "UpdateOrganisationCommand")
    .f(void 0, void 0)
    .ser(se_UpdateOrganisationCommand)
    .de(de_UpdateOrganisationCommand)
    .build() {
}
