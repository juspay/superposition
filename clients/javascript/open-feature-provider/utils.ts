export function getNestedValue(obj: any, key: string): any {
    const keyParts = key.split('.');
    let value = obj;

    for (const part of keyParts) {
        if (value === undefined || value === null || typeof value !== 'object') {
            return undefined;
        }
        value = value[part];
    }

    return value;
}


export function convertToBoolean(value: any, defaultValue: boolean): boolean {
    if (typeof value === 'boolean') return value;
    if (typeof value === 'string') {
        if (value.toLowerCase() === 'true') return true;
        if (value.toLowerCase() === 'false') return false;
    }
    if (typeof value === 'number') return value !== 0;
    return defaultValue;
}

export function convertToString(value: any, defaultValue: string): string {
    if (typeof value === 'string') return value;
    if (value !== undefined && value !== null) return String(value);
    return defaultValue;
}

export function convertToNumber(value: any, defaultValue: number): number {
    if (typeof value === 'number') return value;
    if (typeof value === 'string') {
        const parsed = parseFloat(value);
        if (!isNaN(parsed)) return parsed;
    }
    return defaultValue;
}

export function convertToObject<T>(value: any, defaultValue: T): T {
    if (value !== null && typeof value === 'object') return value as T;
    return defaultValue;
}

// Shared error handling utilities
export function isNetworkError(error: unknown): boolean {
    if (!error) return false;
    
    const errorMessage = error instanceof Error ? error.message : String(error);
    
    return errorMessage.includes('ECONNREFUSED') ||
           errorMessage.includes('ENOTFOUND') ||
           errorMessage.includes('ECONNRESET') ||
           errorMessage.includes('ETIMEDOUT') ||
           errorMessage.includes('fetch failed') ||
           errorMessage.includes('network') ||
           errorMessage.includes('connection');
}

export function categorizeError(error: unknown, endpoint?: string): string {
    if (!error) return 'Unknown error occurred';
    
    const errorMessage = error instanceof Error ? error.message : String(error);
    
    // Bad Request errors (400) - Configuration issues
    if (errorMessage.includes('Bad Request') || errorMessage.includes('parameter org id is required')) {
        return 'Bad Request: Missing or invalid org_id parameter. Please provide a valid org_id in your configuration';
    }
    
    if (errorMessage.includes('parameter workspace_id is required') || errorMessage.includes('parameter workspace id is required')) {
        return 'Bad Request: Missing or invalid workspace_id parameter. Please provide a valid workspace_id in your configuration';
    }
    
    if (errorMessage.includes('not found in organization')) {
        return 'Bad Request: The specified workspace_id does not exist in the given organization. Please check your workspace_id and org_id combination';
    }
    
    // Server configuration issues
    if (errorMessage.includes('Something went wrong')) {
        return 'Server configuration issue: The server responded but encountered an internal error. Check server logs for details';
    }
    
    // Network-related errors
    if (isNetworkError(error)) {
        const endpointMsg = endpoint ? ` at ${endpoint}` : '';
        return `Server connection failed. Please check if the Superposition server is running${endpointMsg}`;
    }
    
    // Authentication errors
    if (errorMessage.includes('401') || errorMessage.includes('403') || errorMessage.includes('unauthorized')) {
        return 'Authentication failed. Please verify your token and credentials';
    }
    
    // Not Found errors (404)
    if (errorMessage.includes('404') || errorMessage.includes('not found')) {
        return 'Resource not found. Please verify your org_id and workspace_id exist on the server';
    }
    
    // Generic error
    return `Request failed: ${errorMessage}`;
}
