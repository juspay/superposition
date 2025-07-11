"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.RampExperimentCommand = exports.$Command = void 0;
const Aws_restJson1_1 = require("../protocols/Aws_restJson1");
const middleware_serde_1 = require("@smithy/middleware-serde");
const smithy_client_1 = require("@smithy/smithy-client");
Object.defineProperty(exports, "$Command", { enumerable: true, get: function () { return smithy_client_1.Command; } });
class RampExperimentCommand extends smithy_client_1.Command.classBuilder()
    .m(function (Command, cs, config, o) {
    return [
        (0, middleware_serde_1.getSerdePlugin)(config, this.serialize, this.deserialize),
    ];
})
    .s("Superposition", "RampExperiment", {})
    .n("SuperpositionClient", "RampExperimentCommand")
    .f(void 0, void 0)
    .ser(Aws_restJson1_1.se_RampExperimentCommand)
    .de(Aws_restJson1_1.de_RampExperimentCommand)
    .build() {
}
exports.RampExperimentCommand = RampExperimentCommand;
