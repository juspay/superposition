const merchantEditableKeys = [
    'VALIDATION_ATTEMPT_THRESHOLD',
    'BENE_VALIDATION_V2_GATEWAY_LIST',
    'BLOCKED_RECON_GATEWAY',
    'FULFILL_ONLY_MAX_LIMIT',
    'ATTEMPT_TO_FINISH',
    'ATTEMPT_RETRYPOLICY',
    'DUMMY_BASE_AMOUNT',
    'ATTEMPT_THRESHOLD',
    'ENABLE_BALANCE_MONITOR',
    'BALANCE_MONITOR_THRESHOLD',
    'BALANCE_MONITOR_DURATION',
    'BALANCE_MONITOR_EMAILS',
    'SR_EMAILS',
    'FILTER_NAME_FOR_FM',
    'OUTAGE_THRESHOLD',
    'DEFAULT_RETURN_URL',
    'WEBHOOK_ENABLED',
    'IP_FILTER_ENABLE',
    'IP_FILTER_LIST',
]

const adminEditableKeys = [
    'JUSPAY_VALIDATION_ACCOUNTS',
    'CUTOVER_TO_HS',
    'ENABLE_CHECK_ON_ORIG_TXN_REF',
    'VALIDATE_ACC_IFSC_VIA_VPA',
    'ENABLE_GETCARD',
    'ALLOWED_BENE_TYPES',
    'CANCEL_ENABLED',
    'GET_BALANCE_RATE_LIMIT',
    'GET_BALANCE_RATE_LIMIT_WINDOW',
    'DASHBOARD_ORDER_CREATION',
    'MERCHANT_FORCE_SYNC_TTL',
    'BENE_VALIDATE_AND_CREATE',
    'BANK_ACCOUNT_FM_MAPPING',
    'ENABLE_ALT_ID_FLOW',
    'FALLBACK_ALT_ID_FLOW',
    'USE_ALT_ID_FOR',
    'ALTID_RETRYPOLICY',
    'ENABLE_VIRTUAL_CARD_NUMBER_FLOW',
    'SMARTCONVERT_RETRY_INTERVAL',
    'RESOLVE_MOBILE_RETRY_INTERVAL',
    'ALLOWED_ORDER_TYPES',
    'DIRECT_WEBHOOK_RESPONSE',
    'ENABLE_TEST_WEBHOOK',
    'CONTROL_SR_FORKFLOW',
    'SHOULD_USE_CASHFREE_V2',
    'RETRY_BEFORE_HALT',
    'RETRY_BEFORE_HALT_FOR_VALIDATION',
    'TEMPTOKEN_RETRYPOLICY',
    'WEBHOOK_RESPONSE_WITH_ADDITIONAL_FIELDS',
]

const completeConfigSchema = {
    FULFILL_ONLY_MAX_LIMIT: { type: 'number' },
    FULFILL_ONLY_MIN_LIMIT: { type: 'number' },
    APPROVE_AND_FULFILL_ONLY_MAX_LIMIT: { type: 'number' },
    FULFILLMENT_BANK_MAP_CASHFREE: {
        type: 'object',
        additionalProperties: { type: 'string' },
    },
    FULFILLMENT_BANK_MAP_ICICI: {
        type: 'object',
        additionalProperties: { type: 'string' },
    },
    FULFILLMENT_BANK_MAP_CFYESB: {
        type: 'object',
        additionalProperties: { type: 'string' },
    },
    FULFILLMENT_BANK_MAP_CFICICI: {
        type: 'object',
        additionalProperties: { type: 'string' },
    },
    FULFILLMENT_BANK_MAP_CFKOTAK: {
        type: 'object',
        additionalProperties: { type: 'string' },
    },
    FULFILLMENT_BANK_MAP_YESBANK: {
        type: 'object',
        additionalProperties: { type: 'string' },
    },
    FULFILLMENT_BANK_MAP_YESNODAL: {
        type: 'object',
        additionalProperties: { type: 'string' },
    },
    FULFILLMENT_BANK_MAP_RAZORPAY: {
        type: 'object',
        additionalProperties: { type: 'string' },
    },
    FULFILLMENT_BANK_MAP_PAYU: {
        type: 'object',
        additionalProperties: { type: 'string' },
    },
    FULFILLMENT_BANK_MAP_BILLDESK: {
        type: 'object',
        additionalProperties: { type: 'string' },
    },
    FULFILLMENT_BANK_MAP_YESBANKFTX: {
        type: 'object',
        additionalProperties: { type: 'string' },
    },
    FULFILLMENT_BANK_MAP_EASEBUZZ: {
        type: 'object',
        additionalProperties: { type: 'string' },
    },
    FULFILLMENT_BANK_MAP_AXIS: {
        type: 'object',
        additionalProperties: { type: 'string' },
    },
    FULFILLMENT_BANK_MAP_IDFC: {
        type: 'object',
        additionalProperties: { type: 'string' },
    },
    FULFILLMENT_BANK_MAP_RBL: {
        type: 'object',
        additionalProperties: { type: 'string' },
    },
    ATTEMPT_RETRYPOLICY: { type: 'array', items: { type: 'number' } },
    CC_VPA_FORMAT: { type: 'object', additionalProperties: { type: 'string' } },
    AUTO_SCHEDULE_ORDER: { type: 'boolean' },
    IP_FILTER_ENABLE: { type: 'boolean' },
    IP_FILTER_LIST: { type: 'array', items: { type: 'string' } },
    ENABLE_GETCARD: { type: 'boolean' },
    SHOULD_SCHEDULE_TASKS: { type: 'boolean' },
    ATTEMPT_THRESHOLD: { type: 'integer' },
    RETRY_BEFORE_HALT: { type: 'array', items: { type: 'number' } },
    WEBLAB_CONFIG: { type: 'array', items: { type: 'object' } },
    WEBHOOK_ENABLED: { type: 'boolean' },
    EXCEPTION_RESCHEDULE_INTERVAL: { type: 'number' },
    REFUND_BLOCKED_LIST: { type: 'array', items: { type: 'string' } },
    ALLOWED_BENE_TYPES: { type: 'array', items: { type: 'string' } },
    DUMMY_BASE_AMOUNT: { type: 'number' },
    DEFAULT_LOCKER_ID: { type: 'string' },
    SHOULD_SHORT_CIRCUIT: { type: 'boolean' },
    ATTEMPT_TO_FINISH: { type: 'boolean' },
    CANCEL_ENABLED: { type: 'boolean' },
    GATEWAY_URLS: { type: 'array', items: { type: 'object' } },
    GATEWAY_CERTPATH: { type: 'array', items: { type: 'object' } },
    GATEWAY_SCHEMA: { type: 'object' },
    SCHEDULE_DELAY: { type: 'number' },
    MERCHANT_CONFIG_SCHEMA: { type: 'object' },
    ADMIN_CONFIG_SCHEMA: { type: 'object' },
    GET_BALANCE_RATE_LIMIT: { type: 'integer' },
    GET_BALANCE_RATE_LIMIT_WINDOW: { type: 'integer' },
    IMPS_STATUS_CHECK_INTERVALS: { type: 'array', items: { type: 'number' } },
    UPI_STATUS_CHECK_INTERVALS: { type: 'array', items: { type: 'number' } },
    NEFT_STATUS_CHECK_INTERVALS: { type: 'array', items: { type: 'number' } },
    VISADIRECT_STATUS_CHECK_INTERVALS: {
        type: 'array',
        items: { type: 'number' },
    },
    JUSPAY_VISADIRECT_STATUS_CHECK_INTERVALS: {
        type: 'array',
        items: { type: 'number' },
    },
    JUSPAY_MASTERSEND_STATUS_CHECK_INTERVALS: {
        type: 'array',
        items: { type: 'number' },
    },
    MASTERSEND_STATUS_CHECK_INTERVALS: {
        type: 'array',
        items: { type: 'number' },
    },
    WALLET_STATUS_CHECK_INTERVALS: { type: 'array', items: { type: 'number' } },
    RTGS_STATUS_CHECK_INTERVALS: { type: 'array', items: { type: 'number' } },
    FT_STATUS_CHECK_INTERVALS: { type: 'array', items: { type: 'number' } },
    DUMMY_STATUS_CHECK_INTERVALS: { type: 'array', items: { type: 'number' } },
    IMPS_STATUS_RECON_INTERVALS: { type: 'array', items: { type: 'number' } },
    UPI_STATUS_RECON_INTERVALS: { type: 'array', items: { type: 'number' } },
    NEFT_STATUS_RECON_INTERVALS: { type: 'array', items: { type: 'number' } },
    VISADIRECT_STATUS_RECON_INTERVALS: {
        type: 'array',
        items: { type: 'number' },
    },
    JUSPAY_VISADIRECT_STATUS_RECON_INTERVALS: {
        type: 'array',
        items: { type: 'number' },
    },
    MASTERSEND_STATUS_RECON_INTERVALS: {
        type: 'array',
        items: { type: 'number' },
    },
    RTGS_STATUS_RECON_INTERVALS: { type: 'array', items: { type: 'number' } },
    FT_STATUS_RECON_INTERVALS: { type: 'array', items: { type: 'number' } },
    BLOCKED_RECON_GATEWAY: { type: 'array', items: { type: 'object' } },
    SHOULD_FAIL: { type: 'boolean' },
    V1_ORDER_STATUS_MERCHANTLIST: { type: 'array', items: { type: 'string' } },
    ENABLE_HASH_RESYNC: { type: 'boolean' },
    BALANCE_MONITOR_THRESHOLD: { type: 'number' },
    BALANCE_MONITOR_EMAILS: { type: 'array', items: { type: 'string' } },
    BALANCE_MONITOR_DURATION: { type: 'integer' },
    ENABLE_BALANCE_MONITOR: { type: 'boolean' },
    QUEUE_PROM_SCHEDULE: { type: 'integer' },
    BYPASS_HEALTH_CHECK: { type: 'boolean' },
    BLOCK_REVERSE_RECON: { type: 'array', items: { type: 'object' } },
    BLOCK_CONFLICT_RECON: { type: 'array', items: { type: 'object' } },
    EC_VERIFICATION_GROUPS: {
        type: 'object',
        additionalProperties: { type: 'string' },
    },
    PROCESSTRACKER_STATUS: { type: 'array', items: { type: 'object' } },
    ENABLE_TOKENIZATION: { type: 'boolean' },
    FALLBACK_TOKENIZATION: { type: 'boolean' },
    SR_EMAILS: { type: 'array', items: { type: 'string' } },
    GLOBAL_SCRIPT: { type: 'string' },
    BENE_VALIDATION_GATEWAY: {
        type: 'object',
        additionalProperties: { type: 'string' },
    },
    BENE_VALIDATION_STATUS_TTL: { type: 'integer' },
    BLACKLIST_SR_EMAILS: { type: 'array', items: { type: 'string' } },
    DASHBOARD_ORDER_CREATION: { type: 'boolean' },
    USE_TOKENISATION_FOR: { type: 'array', items: { type: 'string' } },
    MERCHANT_FORCE_SYNC_TTL: { type: 'integer' },
    BENE_VALIDATE_AND_CREATE: { type: 'boolean' },
    USE_YPAY_SYNC_API: { type: 'object' },
    USE_VALIDATE_ATTEMPT_ON_IDEMPOTENT_REQ: { type: 'object' },
    ENABLE_WEBHOOK_FOR: {
        type: 'object',
        additionalProperties: { type: 'array', items: { type: 'string' } },
    },
    VISA_ELIGIBILITY: { type: 'object' },
    VISA_AQUIRING_BIN: {
        type: 'object',
        additionalProperties: { type: 'string' },
    },
    SHOULD_PASS_TOKEN_DETAILS: { type: 'object' },
    SR_FILENAME_MAPPER: {
        type: 'array',
        items: { type: 'object', additionalProperties: { type: 'string' } },
    },
    INDIVIDUAL_ACCOUNT_THRESHOLD: {
        type: 'object',
        additionalProperties: { type: 'number' },
    },
    BANK_ACCOUNT_FM_MAPPING: {
        type: 'object',
        additionalProperties: { type: 'array', items: { type: 'string' } },
    },
    USE_MERCHANT_SPECIFIC_TR: { type: 'object' },
    YESBANK_DEBIT_STATUS_INTERVAL: { type: 'array', items: { type: 'number' } },
    USE_SR_BASED_ROUTING: { type: 'object' },
    SR_THRESHOLD: { type: 'number' },
    USE_SR_ROUTING_FOR_VALIDATE: { type: 'object' },
    BENE_VALIDATION_GATEWAY_LIST: {
        type: 'object',
        additionalProperties: { type: 'array', items: { type: 'string' } },
    },
    NON_DIRECT_FAILURE_BANKS: {
        type: 'object',
        additionalProperties: { type: 'array', items: { type: 'string' } },
    },
    RESELLER_WHITELISTED_MERCHANTS: {
        type: 'object',
        additionalProperties: { type: 'array', items: { type: 'string' } },
    },
    MASTERSEND_ELIGIBILITY: { type: 'object' },
    JUSPAY_MASTERSEND_ACQUIRER_DETAILS: {
        type: 'object',
        additionalProperties: {
            type: 'object',
            additionalProperties: { type: 'string' },
        },
    },
    OUTAGE_THRESHOLD: { type: 'object' },
    GATEWAY_SCORE_THRESHOLD: { type: 'number' },
    VALIDATION_UPI_CHECK_STATUS_INTERVAL: {
        type: 'array',
        items: { type: 'number' },
    },
    VALIDATION_IMPS_CHECK_STATUS_INTERVAL: {
        type: 'array',
        items: { type: 'number' },
    },
    VALIDATION_NEFT_CHECK_STATUS_INTERVAL: {
        type: 'array',
        items: { type: 'number' },
    },
    VALIDATION_RTGS_CHECK_STATUS_INTERVAL: {
        type: 'array',
        items: { type: 'number' },
    },
    USE_RESOLVE_MOBILE_NUMBER: { type: 'object' },
    ALLOWED_V2_BULK_ORDER_CREATE: { type: 'object' },
    VALIDATE_ACC_IFSC_VIA_VPA: { type: 'boolean' },
    SHOULD_VALIDATE_THR_EULER: { type: 'object' },
    SHOULD_MAKE_CREDS_ON_VDMS: {
        type: 'object',
        additionalProperties: { type: 'array', items: { type: 'string' } },
    },
    BLOCKED_FM_FOR_TXN_FORCE_SYNC: { type: 'array', items: { type: 'object' } },
    PRIORITY_LOGIC_CONFIGS: {
        type: 'object',
        additionalProperties: { type: 'array', items: { type: 'string' } },
    },
    CONTEXT_WHITELISTED_URLS: {
        type: 'object',
        additionalProperties: { type: 'array', items: { type: 'string' } },
    },
    ACCOUNT_IFSC_VALIDATION_VIA_UPI: { type: 'object' },
    ELIGIBLE_BANKS_FOR_ACCOUNT_IFSC_VALIDATION_VIA_UPI: {
        type: 'array',
        items: { type: 'string' },
    },
    FILTER_NAME_FOR_FM: { type: 'array', items: { type: 'object' } },
    ENABLE_VIRTUAL_CARD_NUMBER_FLOW: { type: 'boolean' },
    ELIGIBLE_BANKS_FOR_VIRTUAL_CARD_NUMBER: {
        type: 'array',
        items: { type: 'string' },
    },
    ENABLE_ALT_ID_FLOW: { type: 'number' },
    FALLBACK_ALT_ID_FLOW: { type: 'boolean' },
    USE_ALT_ID_FOR: { type: 'array', items: { type: 'string' } },
    ALTID_RETRYPOLICY: { type: 'array', items: { type: 'number' } },
    CONSUME_GLOBAL_SR: { type: 'boolean' },
    LIST_OF_PII: {
        type: 'object',
        additionalProperties: { type: 'array', items: { type: 'string' } },
    },
    ENABLE_CHECK_ON_ORIG_TXN_REF: { type: 'boolean' },
    SMARTCONVERT_RETRY_INTERVAL: { type: 'array', items: { type: 'number' } },
    SHOULD_ALLOW_RESOLVE_AND_PAYOUT: { type: 'object' },
    MOBILE_NUMBER_PAYOUT: { type: 'object' },
    RESOLVE_MOBILE_RETRY_INTERVAL: { type: 'array', items: { type: 'number' } },
    CONFIGURATION_SCHEMA: { type: 'array', items: { type: 'object' } },
    TEMPTOKEN_RETRYPOLICY: { type: 'array', items: { type: 'number' } },
    SHOULD_BYPASS_GLOBAL_WEBLAB: { type: 'object' },
    OUTAGE_DATA_PERSISTANCE_DURATION: { type: 'integer' },
    SHOULD_SEND_FPI_TO_VISA: { type: 'object' },
    CUTOVER_TO_HS: { type: 'object' },
    MS_LIMIT_THRESHOLD: { type: 'number' },
    ALLOWED_ORDER_TYPES: { type: 'array', items: { type: 'object' } },
    WHITELISTED_TENANTS: { type: 'array', items: { type: 'string' } },
    DIRECT_WEBHOOK_RESPONSE: { type: 'boolean' },
    VALIDATION_ATTEMPT_THRESHOLD: { type: 'integer' },
    IS_ALLOWED_TO_USE_JUSPAY_ACCNTS: { type: 'object' },
    JUSPAY_VALIDATION_ACCOUNTS: { type: 'array', items: { type: 'string' } },
    ELIGIBLE_BANKS_FOR_PENNYLESS_ACC_VERIFICATION: {
        type: 'array',
        items: { type: 'string' },
    },
    ELIGIBLE_BANKS_FOR_UPIACCIFSC_VERIFICATION: {
        type: 'array',
        items: { type: 'string' },
    },
    BENE_VALIDATION_V2_GATEWAY_LIST: {
        type: 'array',
        items: { type: 'string' },
    },
    SR_THRESHOLD_FOR_VALIDATION: { type: 'number' },
    SR_DOWNGRADE_FACTOR_FOR_VALIDATION: { type: 'number' },
    SR_UPGRADE_FACTOR_FOR_VALIDATION: { type: 'number' },
    SHOULD_RUN_VAL_RETRY_IN_SYNC: { type: 'object' },
    ENABLE_TEST_WEBHOOK: { type: 'boolean' },
    IDFC_STATUS_CHECK_INTERVALS: { type: 'array', items: { type: 'number' } },
    CONTROL_SR_FORKFLOW: { type: 'number' },
    GATEWAY_SCORE_THRESHOLD_FOR_VALIDATION: { type: 'number' },
    ENABLED_FOR_AUTHTOKEN: { type: 'object' },
    RETRY_BEFORE_HALT_FOR_VALIDATION: {
        type: 'array',
        items: { type: 'number' },
    },
    PII_ROTATION_FETCH_LIMIT: { type: 'integer' },
    DATA_ROTATION_BATCH: {
        type: 'object',
        additionalProperties: { type: 'integer' },
    },
    DATA_MASKING_BATCH: {
        type: 'object',
        additionalProperties: { type: 'integer' },
    },
    SHOULD_USE_CASHFREE_V2: { type: 'number' },
    USE_LONG_PAYOUT_LINK: { type: 'boolean' },
    WEBHOOK_RESPONSE_WITH_ADDITIONAL_FIELDS: { type: 'boolean' },
    RISK_THRESHOLD: { type: 'integer' },
    DEFAULT_RETURN_URL: { type: 'string' }, // Added this as it was in merchant editable keys but not in the complete schema
}

const globalKeys = Object.keys(completeConfigSchema).filter(
    (key) =>
        !merchantEditableKeys.includes(key) && !adminEditableKeys.includes(key),
)

function createDefaultValue(schema) {
    switch (schema.type) {
        case 'string':
            return ''
        case 'number':
        case 'integer':
            return 0
        case 'boolean':
            return false
        case 'array':
            if (
                schema.items.type === 'number' ||
                schema.items.type === 'integer'
            ) {
                return [0]
            } else if (schema.items.type === 'string') {
                return ['']
            } else if (schema.items.type === 'object') {
                return [{}]
            } else {
                return []
            }
        case 'object':
            return {}
        default:
            return null
    }
}

// Create payload objects for each category
function createConfigPayloads(keyList, category) {
    return keyList
        .map((key) => {
            if (!completeConfigSchema[key]) {
                console.warn(`Schema not found for key: ${key} in ${category}`)
                return null
            }

            return {
                key: key,
                schema: completeConfigSchema[key],
                value: createDefaultValue(completeConfigSchema[key]),
                description: key,
                change_reason: key,
            }
        })
        .filter((payload) => payload !== null)
}

// Prepare the payloads for each category
const merchantConfigPayloads = createConfigPayloads(
    merchantEditableKeys,
    'merchant',
)
const adminConfigPayloads = createConfigPayloads(adminEditableKeys, 'admin')
const globalConfigPayloads = createConfigPayloads(globalKeys, 'global')

// Function to send HTTP requests for creating config keys
async function createConfigKeys(payloads, configType) {
    const url = 'http://127.0.0.1:8080/default-config' // Replace with your actual API endpoint

    let tenantHeader
    switch (configType) {
        case 'merchant':
            tenantHeader = 'morpheusMerchantConfig'
            break
        case 'admin':
            tenantHeader = 'morpheusAdminConfig'
            break
        case 'global':
            tenantHeader = 'morpheusGlobalConfig'
            break
        default:
            throw new Error(`Invalid config type: ${configType}`)
    }

    const results = []

    for (const payload of payloads) {
        try {
            const response = await fetch(url, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                    'x-tenant': tenantHeader,
                    'x-org-id': 'localorg'
                },
                body: JSON.stringify(payload),
            })

            const result = await response.json()
            results.push({
                key: payload.name,
                success: response.ok,
                data: result,
            })

            console.log(`Created ${configType} config key: ${payload.name}`)
        } catch (error) {
            results.push({
                key: payload.name,
                success: false,
                error: error.message,
            })

            console.error(
                `Error creating ${configType} config key ${payload.name}:`,
                error,
            )
        }
    }

    return results
}

async function createAllConfigKeys() {
    console.log('Creating merchant config keys...')
    const merchantResults = await createConfigKeys(
        merchantConfigPayloads,
        'merchant',
    )

    console.log('Creating admin config keys...')
    const adminResults = await createConfigKeys(adminConfigPayloads, 'admin')

    console.log('Creating global config keys...')
    const globalResults = await createConfigKeys(globalConfigPayloads, 'global')

    return {
        merchant: merchantResults,
        admin: adminResults,
        global: globalResults,
    }
}

createAllConfigKeys().then((results) =>
    console.log('All config keys created:', results),
)

console.log(`Merchant Editable Keys: ${merchantEditableKeys.length}`)
console.log(`Admin Editable Keys: ${adminEditableKeys.length}`)
console.log(`Global Keys: ${globalKeys.length}`)
console.log(
    `Total Keys: ${merchantEditableKeys.length + adminEditableKeys.length + globalKeys.length}`,
)
console.log(`Complete Schema Keys: ${Object.keys(completeConfigSchema).length}`)
