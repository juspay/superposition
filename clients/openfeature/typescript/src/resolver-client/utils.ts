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
