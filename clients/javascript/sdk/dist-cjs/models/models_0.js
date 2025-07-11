"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.WorkspaceNotFound = exports.WebhookNotFound = exports.OrganisationNotFound = exports.Stage = exports.FunctionExecutionRequest = exports.ExperimentSortOn = exports.ExperimentGroupSortOn = exports.TypeTemplatesNotFound = exports.FunctionNotFound = exports.WorkspaceStatus = exports.Version = exports.HttpMethod = exports.OrgStatus = exports.FunctionTypes = exports.ContextFilterSortOn = exports.SortBy = exports.MergeStrategy = exports.ExperimentStatusType = exports.ExperimentType = exports.ContextActionOut = exports.ContextAction = exports.ContextIdentifier = exports.VariantType = exports.ResourceNotFound = exports.InternalServerError = void 0;
const SuperpositionServiceException_1 = require("./SuperpositionServiceException");
class InternalServerError extends SuperpositionServiceException_1.SuperpositionServiceException {
    name = "InternalServerError";
    $fault = "server";
    constructor(opts) {
        super({
            name: "InternalServerError",
            $fault: "server",
            ...opts
        });
        Object.setPrototypeOf(this, InternalServerError.prototype);
    }
}
exports.InternalServerError = InternalServerError;
class ResourceNotFound extends SuperpositionServiceException_1.SuperpositionServiceException {
    name = "ResourceNotFound";
    $fault = "client";
    constructor(opts) {
        super({
            name: "ResourceNotFound",
            $fault: "client",
            ...opts
        });
        Object.setPrototypeOf(this, ResourceNotFound.prototype);
    }
}
exports.ResourceNotFound = ResourceNotFound;
exports.VariantType = {
    CONTROL: "CONTROL",
    EXPERIMENTAL: "EXPERIMENTAL",
};
var ContextIdentifier;
(function (ContextIdentifier) {
    ContextIdentifier.visit = (value, visitor) => {
        if (value.id !== undefined)
            return visitor.id(value.id);
        if (value.context !== undefined)
            return visitor.context(value.context);
        return visitor._(value.$unknown[0], value.$unknown[1]);
    };
})(ContextIdentifier || (exports.ContextIdentifier = ContextIdentifier = {}));
var ContextAction;
(function (ContextAction) {
    ContextAction.visit = (value, visitor) => {
        if (value.PUT !== undefined)
            return visitor.PUT(value.PUT);
        if (value.REPLACE !== undefined)
            return visitor.REPLACE(value.REPLACE);
        if (value.DELETE !== undefined)
            return visitor.DELETE(value.DELETE);
        if (value.MOVE !== undefined)
            return visitor.MOVE(value.MOVE);
        return visitor._(value.$unknown[0], value.$unknown[1]);
    };
})(ContextAction || (exports.ContextAction = ContextAction = {}));
var ContextActionOut;
(function (ContextActionOut) {
    ContextActionOut.visit = (value, visitor) => {
        if (value.PUT !== undefined)
            return visitor.PUT(value.PUT);
        if (value.REPLACE !== undefined)
            return visitor.REPLACE(value.REPLACE);
        if (value.DELETE !== undefined)
            return visitor.DELETE(value.DELETE);
        if (value.MOVE !== undefined)
            return visitor.MOVE(value.MOVE);
        return visitor._(value.$unknown[0], value.$unknown[1]);
    };
})(ContextActionOut || (exports.ContextActionOut = ContextActionOut = {}));
exports.ExperimentType = {
    DEFAULT: "DEFAULT",
    DELETE_OVERRIDES: "DELETE_OVERRIDES",
};
exports.ExperimentStatusType = {
    CONCLUDED: "CONCLUDED",
    CREATED: "CREATED",
    DISCARDED: "DISCARDED",
    INPROGRESS: "INPROGRESS",
    PAUSED: "PAUSED",
};
exports.MergeStrategy = {
    MERGE: "MERGE",
    REPLACE: "REPLACE",
};
exports.SortBy = {
    Asc: "asc",
    Desc: "desc",
};
exports.ContextFilterSortOn = {
    CreatedAt: "created_at",
    LastModifiedAt: "last_modified_at",
    Weight: "weight",
};
exports.FunctionTypes = {
    Autocomplete: "AUTOCOMPLETE",
    Validation: "VALIDATION",
};
exports.OrgStatus = {
    Active: "Active",
    Inactive: "Inactive",
    PendingKyb: "PendingKyb",
};
exports.HttpMethod = {
    DELETE: "DELETE",
    GET: "GET",
    HEAD: "HEAD",
    PATCH: "PATCH",
    POST: "POST",
    PUT: "PUT",
};
exports.Version = {
    V1: "V1",
};
exports.WorkspaceStatus = {
    DISABLED: "DISABLED",
    ENABLED: "ENABLED",
};
class FunctionNotFound extends SuperpositionServiceException_1.SuperpositionServiceException {
    name = "FunctionNotFound";
    $fault = "client";
    constructor(opts) {
        super({
            name: "FunctionNotFound",
            $fault: "client",
            ...opts
        });
        Object.setPrototypeOf(this, FunctionNotFound.prototype);
    }
}
exports.FunctionNotFound = FunctionNotFound;
class TypeTemplatesNotFound extends SuperpositionServiceException_1.SuperpositionServiceException {
    name = "TypeTemplatesNotFound";
    $fault = "client";
    constructor(opts) {
        super({
            name: "TypeTemplatesNotFound",
            $fault: "client",
            ...opts
        });
        Object.setPrototypeOf(this, TypeTemplatesNotFound.prototype);
    }
}
exports.TypeTemplatesNotFound = TypeTemplatesNotFound;
exports.ExperimentGroupSortOn = {
    CreatedAt: "created_at",
    LastModifiedAt: "last_modified_at",
    Name: "name",
};
exports.ExperimentSortOn = {
    CreatedAt: "created_at",
    LastModifiedAt: "last_modified_at",
};
var FunctionExecutionRequest;
(function (FunctionExecutionRequest) {
    FunctionExecutionRequest.visit = (value, visitor) => {
        if (value.ValidateFunctionRequest !== undefined)
            return visitor.ValidateFunctionRequest(value.ValidateFunctionRequest);
        if (value.AutocompleteFunctionRequest !== undefined)
            return visitor.AutocompleteFunctionRequest(value.AutocompleteFunctionRequest);
        return visitor._(value.$unknown[0], value.$unknown[1]);
    };
})(FunctionExecutionRequest || (exports.FunctionExecutionRequest = FunctionExecutionRequest = {}));
exports.Stage = {
    DRAFT: "draft",
    PUBLISHED: "published",
};
class OrganisationNotFound extends SuperpositionServiceException_1.SuperpositionServiceException {
    name = "OrganisationNotFound";
    $fault = "client";
    constructor(opts) {
        super({
            name: "OrganisationNotFound",
            $fault: "client",
            ...opts
        });
        Object.setPrototypeOf(this, OrganisationNotFound.prototype);
    }
}
exports.OrganisationNotFound = OrganisationNotFound;
class WebhookNotFound extends SuperpositionServiceException_1.SuperpositionServiceException {
    name = "WebhookNotFound";
    $fault = "client";
    constructor(opts) {
        super({
            name: "WebhookNotFound",
            $fault: "client",
            ...opts
        });
        Object.setPrototypeOf(this, WebhookNotFound.prototype);
    }
}
exports.WebhookNotFound = WebhookNotFound;
class WorkspaceNotFound extends SuperpositionServiceException_1.SuperpositionServiceException {
    name = "WorkspaceNotFound";
    $fault = "client";
    constructor(opts) {
        super({
            name: "WorkspaceNotFound",
            $fault: "client",
            ...opts
        });
        Object.setPrototypeOf(this, WorkspaceNotFound.prototype);
    }
}
exports.WorkspaceNotFound = WorkspaceNotFound;
