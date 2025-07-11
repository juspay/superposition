"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.convertToObject = exports.convertToNumber = exports.convertToString = exports.convertToBoolean = exports.getNestedValue = void 0;
function getNestedValue(obj, key) {
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
exports.getNestedValue = getNestedValue;
function convertToBoolean(value, defaultValue) {
    if (typeof value === 'boolean')
        return value;
    if (typeof value === 'string') {
        if (value.toLowerCase() === 'true')
            return true;
        if (value.toLowerCase() === 'false')
            return false;
    }
    if (typeof value === 'number')
        return value !== 0;
    return defaultValue;
}
exports.convertToBoolean = convertToBoolean;
function convertToString(value, defaultValue) {
    if (typeof value === 'string')
        return value;
    if (value !== undefined && value !== null)
        return String(value);
    return defaultValue;
}
exports.convertToString = convertToString;
function convertToNumber(value, defaultValue) {
    if (typeof value === 'number')
        return value;
    if (typeof value === 'string') {
        const parsed = parseFloat(value);
        if (!isNaN(parsed))
            return parsed;
    }
    return defaultValue;
}
exports.convertToNumber = convertToNumber;
function convertToObject(value, defaultValue) {
    if (value !== null && typeof value === 'object')
        return value;
    return defaultValue;
}
exports.convertToObject = convertToObject;
