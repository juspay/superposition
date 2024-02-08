import * as jsonLogic from 'json-logic-js';
import { deepMerge } from './utils/deepMerge';
import { compareSemanticIsGreater } from './utils/operations'
import { IObject, Dimension } from './types'

type DataFromCacApi = {
    contexts: Array<Dimension>;
    overrides: IObject;
    default_configs: IObject;
}

export class CacReader {
    contexts: Array<Dimension>;
    overrides: IObject;
    defaultConfig: IObject;

    static {
        jsonLogic.add_operation(">>", compareSemanticIsGreater);
    }

    constructor(completeConfig: DataFromCacApi) {
        this.contexts = completeConfig.contexts;
        this.overrides = completeConfig.overrides;
        this.defaultConfig = completeConfig.default_configs;
    }

    public evaluateConfig(data: IObject): IObject {

        const requiredOverrides: Array<IObject> = [];
        for (let i = 0; i < this.contexts.length; i++) {
            if (jsonLogic.apply(this.contexts[i].condition, data)) {
                requiredOverrides.push(
                    ...this.contexts[i].override_with_keys.map(x => this.overrides[x])
                );
            }
        }

        const targetConfig: IObject = { ...this.defaultConfig };
        return deepMerge(targetConfig, ...requiredOverrides);
    }
}
