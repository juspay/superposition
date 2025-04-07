import { de_GetOrganisationCommand, se_GetOrganisationCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class GetOrganisationCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "GetOrganisation", {})
    .n("SuperpositionClient", "GetOrganisationCommand")
    .f(void 0, void 0)
    .ser(se_GetOrganisationCommand)
    .de(de_GetOrganisationCommand)
    .build() {
}
