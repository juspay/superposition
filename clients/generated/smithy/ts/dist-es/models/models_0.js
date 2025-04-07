import { SuperpositionServiceException as __BaseException } from "./SuperpositionServiceException";
export const VariantType = {
    CONTROL: "CONTROL",
    EXPERIMENTAL: "EXPERIMENTAL",
};
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
export const ExperimentStatusType = {
    CONCLUDED: "CONCLUDED",
    CREATED: "CREATED",
    DISCARDED: "DISCARDED",
    INPROGRESS: "INPROGRESS",
};
export const MergeStrategy = {
    MERGE: "MERGE",
    REPLACE: "REPLACE",
};
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
