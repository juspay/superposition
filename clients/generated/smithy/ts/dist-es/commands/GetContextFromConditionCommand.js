import { de_GetContextFromConditionCommand, se_GetContextFromConditionCommand, } from "../protocols/Aws_restJson1";
import { getSerdePlugin } from "@smithy/middleware-serde";
import { Command as $Command } from "@smithy/smithy-client";
export { $Command };
export class GetContextFromConditionCommand extends $Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        getSerdePlugin(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "GetContextFromCondition", {})
    .n("SuperpositionClient", "GetContextFromConditionCommand")
    .f(void 0, void 0)
    .ser(se_GetContextFromConditionCommand)
    .de(de_GetContextFromConditionCommand)
    .build() {
}
