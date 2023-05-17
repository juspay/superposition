import * as jsonLogic from 'json-logic-js';
import {deepMerge} from './utils/deepMerge';
import {compareSemanticIsGreater} from './utils/operations'
import {IObject, Dimension, DimensionConfig} from './types'


export class Config{
  
  dimension : Array<Dimension>;
  overrides : IObject;
  defaultConfig : IObject;
  
  
  static {
    jsonLogic.add_operation(">>",compareSemanticIsGreater); 
  }

  constructor(dimension : DimensionConfig, overrides : IObject, defaultConfig : IObject) {
    this.dimension = dimension.dimensions;
    this.overrides = overrides;
    this.defaultConfig = defaultConfig;
  }


  public evaluateConfig(data : IObject) : IObject {

    const requiredOverrides : Array<IObject> = [];
    for(let i = 0; i < this.dimension.length; i++) {
      if(jsonLogic.apply(this.dimension[i].condition, data)) {
        requiredOverrides.push(
          ...this.dimension[i].overrideWithKeys.map(x => this.overrides[x])
        ); 
      }
    }
    
    const targetConfig : IObject = {...this.defaultConfig};
    return deepMerge(targetConfig , ...requiredOverrides);
  } 
}
