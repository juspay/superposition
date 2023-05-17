
export type IObject = {
  [key: string]: any;
}

export type Dimension = {
  condition : IObject,
  overrideWithKeys : Array<string> 
}

export type DimensionConfig = {
  dimensions : Array<Dimension>
};


export type IIsObject = {
  (item: any): boolean;
}

