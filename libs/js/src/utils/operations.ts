const compareSemanticIsGreaterImp = function (version_a: string, version_b: string) {
    return version_a.localeCompare(version_b, undefined, {numeric:true, sensitivity:'base'}) > 0;
}

export const compareSemanticIsGreater = function (version_a: string, version_b: string, version_c?: string) {
    return (version_c === undefined ?
        compareSemanticIsGreaterImp(version_a, version_b) :
        (compareSemanticIsGreaterImp(version_a, version_b) && compareSemanticIsGreaterImp(version_b, version_c)));
}
