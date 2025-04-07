import { de_CreateOrganisationCommand, se_CreateOrganisationCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class CreateOrganisationCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "CreateOrganisation", {})
    .n("SuperpositionClient", "CreateOrganisationCommand")
    .f(void 0, void 0)
    .ser(se_CreateOrganisationCommand)
    .de(de_CreateOrganisationCommand)
    .build() {
}
