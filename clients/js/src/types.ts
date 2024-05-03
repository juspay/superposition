export type IObject = {
    [key: string]: any;
}

export type Dimension = {
    condition: IObject,
    override_with_keys: Array<string>
}

export type DimensionConfig = {
    dimensions: Array<Dimension>
};

export type IIsObject = {
    (item: any): boolean;
}

export enum VariantType {
    CONTROL = "CONTROL",
    EXPERIMENTAL = "EXPERIMENTAL",
}

export type Variant = {
    id: String,
    overrides: Object,
    variant_type: VariantType,
}

export type Variants = Array<Variant>;

export enum ExperimentStatusType {
    CREATED = "CREATED",
    INPROGRESS = "INPROGRESS",
    CONCLUDED = "CONCLUDED",
}

export type Experiment = {
    variants: Variants,
    name: String,
    id: String,
    traffic_percentage: number,
    context: Object,
    status: ExperimentStatusType,
}

export type Experiments = Array<Experiment>;
