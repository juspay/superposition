const {Config} = require('../build/index');

const defaultConfig = require('../samples/default_config.json');
const overrides = require('../samples/overrides.json');
const dimensions = require('../samples/dimensions.json')


const data = {
    tier : "2",
    merchantId : "zee5"
}
const configEvaluator = new Config(dimensions, overrides, defaultConfig);
const result = configEvaluator.evaluateConfig(data);

console.log(JSON.stringify(result));
