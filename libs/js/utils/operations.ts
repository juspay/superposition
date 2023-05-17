const compareSemanticIsGreaterImp = function(version_a : string, version_b : string) {
  const x = version_a.split(".");
  const y = version_b.split(".");
  for(let i = 0; i < x.length; i++) {
    if(x[i] > y[i])
      return true;
  }
  return false;
}

export const compareSemanticIsGreater = function(version_a : string, version_b: string, version_c ?: string) {
  return (version_c === undefined ?
    compareSemanticIsGreaterImp(version_a, version_b) :
    (compareSemanticIsGreaterImp(version_a, version_b) && compareSemanticIsGreaterImp(version_b, version_c)));
}
