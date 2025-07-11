import { SuperpositionServiceException as __BaseException } from "./SuperpositionServiceException";
export class InternalServerError extends __BaseException {
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
export class ResourceNotFound extends __BaseException {
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
export const VariantType = {
    CONTROL: "CONTROL",
    EXPERIMENTAL: "EXPERIMENTAL",
};
export var ContextIdentifier;
(function (ContextIdentifier) {
    ContextIdentifier.visit = (value, visitor) => {
        if (value.id !== undefined)
            return visitor.id(value.id);
        if (value.context !== undefined)
            return visitor.context(value.context);
        return visitor._(value.$unknown[0], value.$unknown[1]);
    };
})(ContextIdentifier || (ContextIdentifier = {}));
export var ContextAction;
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
})(ContextAction || (ContextAction = {}));
export var ContextActionOut;
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
})(ContextActionOut || (ContextActionOut = {}));
export const ExperimentType = {
    DEFAULT: "DEFAULT",
    DELETE_OVERRIDES: "DELETE_OVERRIDES",
};
export const ExperimentStatusType = {
    CONCLUDED: "CONCLUDED",
    CREATED: "CREATED",
    DISCARDED: "DISCARDED",
    INPROGRESS: "INPROGRESS",
    PAUSED: "PAUSED",
};
export const MergeStrategy = {
    MERGE: "MERGE",
    REPLACE: "REPLACE",
};
export const SortBy = {
    Asc: "asc",
    Desc: "desc",
};
export const ContextFilterSortOn = {
    CreatedAt: "created_at",
    LastModifiedAt: "last_modified_at",
    Weight: "weight",
};
export const FunctionTypes = {
    Autocomplete: "AUTOCOMPLETE",
    Validation: "VALIDATION",
};
export const OrgStatus = {
    Active: "Active",
    Inactive: "Inactive",
    PendingKyb: "PendingKyb",
};
export const HttpMethod = {
    DELETE: "DELETE",
    GET: "GET",
    HEAD: "HEAD",
    PATCH: "PATCH",
    POST: "POST",
    PUT: "PUT",
};
export const Version = {
    V1: "V1",
};
export const WorkspaceStatus = {
    DISABLED: "DISABLED",
    ENABLED: "ENABLED",
};
export class FunctionNotFound extends __BaseException {
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
export class TypeTemplatesNotFound extends __BaseException {
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
export const ExperimentGroupSortOn = {
    CreatedAt: "created_at",
    LastModifiedAt: "last_modified_at",
    Name: "name",
};
export const ExperimentSortOn = {
    CreatedAt: "created_at",
    LastModifiedAt: "last_modified_at",
};
export var FunctionExecutionRequest;
(function (FunctionExecutionRequest) {
    FunctionExecutionRequest.visit = (value, visitor) => {
        if (value.ValidateFunctionRequest !== undefined)
            return visitor.ValidateFunctionRequest(value.ValidateFunctionRequest);
        if (value.AutocompleteFunctionRequest !== undefined)
            return visitor.AutocompleteFunctionRequest(value.AutocompleteFunctionRequest);
        return visitor._(value.$unknown[0], value.$unknown[1]);
    };
})(FunctionExecutionRequest || (FunctionExecutionRequest = {}));
export const Stage = {
    DRAFT: "draft",
    PUBLISHED: "published",
};
export class OrganisationNotFound extends __BaseException {
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
export class WebhookNotFound extends __BaseException {
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
export class WorkspaceNotFound extends __BaseException {
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
