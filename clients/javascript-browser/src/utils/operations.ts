const compareJPVersion = function (version_a: string, version_b: string): number {
    return version_a.localeCompare(version_b, undefined, { numeric: true, sensitivity: 'base' });
}

export const isJPVersionEqual = function (version_a: string, version_b: string): boolean {
    return compareJPVersion(version_a, version_b) === 0;
}

export const isJPVersionGreater = function (version_a: string, version_b: string): boolean {
    return compareJPVersion(version_a, version_b) > 0;
}

export const isJPVersionGreaterEqual = function (version_a: string, version_b: string): boolean {
    return compareJPVersion(version_a, version_b) >= 0;
}

export const isJPVersionLesser = function (version_a: string, version_b: string, version_c?: string): boolean {
    return (version_c === undefined ?
        compareJPVersion(version_a, version_b) < 0 :
        (compareJPVersion(version_a, version_b) < 0 && compareJPVersion(version_b, version_c) < 0));
}

export const isJPVersionLesserEqual = function (version_a: string, version_b: string, version_c?: string): boolean {
    return (version_c === undefined ?
        compareJPVersion(version_a, version_b) <= 0 :
        (compareJPVersion(version_a, version_b) <= 0 && compareJPVersion(version_b, version_c) <= 0))
}

export const matchRegex = function (text: string, pattern: string, flag?: string): boolean {
    const re = new RegExp(pattern, flag);
    return re.test(text);
}
