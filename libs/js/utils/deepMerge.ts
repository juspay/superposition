import {IIsObject, IObject} from '../types'


const isObject: IIsObject = (item: any): boolean => {
  return (item === Object(item) && !Array.isArray(item));
};

export const deepMerge = (target: IObject, ...sources: Array<IObject>) : IObject => {
  // return the target if no sources passed
  if (!sources.length) {
    return target;
  }

  const result: IObject = target;

  if (isObject(result)) {
    const len: number = sources.length;

    for (let i = 0; i < len; i += 1) {
      const elm: any = sources[i];

      if (isObject(elm)) {
        for (const key in elm) {
          if (elm.hasOwnProperty(key)) {
            if (isObject(elm[key])) {
              if (!result[key] || !isObject(result[key])) {
                result[key] = {};
              }
              deepMerge(result[key], elm[key]);
            } else {
              if (Array.isArray(result[key]) && Array.isArray(elm[key])) {
                result[key] = [...elm[key]];
              } else {
                result[key] = elm[key];
              }
            }
          }
        }
      }
    }
  }

  return result;
};
