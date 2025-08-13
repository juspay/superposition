/**
 * Field Utility Functions
 * Helper functions for field parsing and validation
 */

export class FieldUtils {
  /**
   * Parse resolved configuration to field objects
   * @param {Object} resolvedConfig - Configuration from Superposition
   * @returns {Array} Array of field objects
   */
  static parseResolvedConfigToFields(resolvedConfig) {
    const fields = [];
    const processedFields = new Set();

    // Extract base field names from metadata keys
    const baseFields = new Set();
    for (const key of Object.keys(resolvedConfig)) {
      if (key.includes('._')) {
        const baseField = key.split('._')[0];
        baseFields.add(baseField);
      }
    }

    console.log('Base fields found:', Array.from(baseFields));

    for (const baseField of baseFields) {
      // Skip if already processed
      if (processedFields.has(baseField)) continue;
      
      const displayName = resolvedConfig[`${baseField}._display_name`] || this.formatDisplayName(baseField);
      const fieldType = resolvedConfig[`${baseField}._field_type`] || 'text_input';
      const required = resolvedConfig[`${baseField}._required`] || false;
      const options = resolvedConfig[`${baseField}._options`] || [];
      const outputPath = resolvedConfig[`${baseField}._output_path`] || baseField;
      const defaultValue = resolvedConfig[`${baseField}._default_value`] || '';

      // Skip fields that are not required (_required: false)
      if (!required) {
        continue;
      }

      // Determine component based on field path
      const component = this.determineComponent(baseField);
      
      // Auto-convert field types for better UX
      const enhancedFieldType = this.enhanceFieldType(fieldType, baseField, component);
      
      fields.push({
        name: baseField,
        display_name: displayName,
        type: enhancedFieldType,
        required: required,
        component: component,
        options: options,
        output_path: outputPath,
        default_value: defaultValue
      });
      
      processedFields.add(baseField);
    }
    
    return fields;
  }

  /**
   * Enhance field type for better UX
   * @param {string} fieldType - Original field type
   * @param {string} baseField - Base field name
   * @param {string} component - Component type
   * @returns {string} Enhanced field type
   */
  static enhanceFieldType(fieldType, baseField, component) {
    // Auto-convert state fields to state_select
    if (fieldType === 'text_input' && (baseField.includes('.state') || baseField.includes('.province'))) {
      return 'state_select';
    }
    
    // Auto-convert country fields to country_select if not already
    if (fieldType === 'text_input' && baseField.includes('.country')) {
      return 'country_select';
    }
    
    // Auto-convert phone country code fields
    if (fieldType === 'text_input' && baseField.includes('.country_code')) {
      return 'country_code_select';
    }
    
    // Auto-convert phone number fields
    if (fieldType === 'text_input' && (baseField.includes('.phone') || baseField.includes('.number')) && component === 'billing') {
      return 'phone_input';
    }
    
    return fieldType;
  }

  /**
   * Determine component type based on field name
   * @param {string} baseField - Base field name
   * @returns {string} Component type
   */
  static determineComponent(baseField) {
    if (baseField.startsWith('card.')) {
      return 'card';
    } else if (baseField.startsWith('billing.')) {
      return 'billing';
    } else if (baseField.startsWith('shipping.')) {
      return 'shipping';
    } else if (baseField.startsWith('bank_debit.') || baseField.startsWith('bank_redirect.') || baseField.startsWith('bank_transfer.')) {
      return 'bank';
    } else if (baseField.startsWith('wallet.')) {
      return 'wallet';
    } else if (baseField.startsWith('crypto.')) {
      return 'crypto';
    } else if (baseField.startsWith('upi.')) {
      return 'upi';
    } else if (baseField.startsWith('voucher.')) {
      return 'voucher';
    } else if (baseField.startsWith('gift_card.')) {
      return 'gift_card';
    } else if (baseField.startsWith('mobile_payment.')) {
      return 'mobile_payment';
    } else if (baseField === 'email') {
      return 'billing';
    } else if (baseField.startsWith('order_details.')) {
      return 'other';
    }
    return 'other';
  }

  /**
   * Format display name from field key
   * @param {string} key - Field key
   * @returns {string} Formatted display name
   */
  static formatDisplayName(key) {
    return key
      .split('.')
      .pop()
      .replace(/_/g, ' ')
      .replace(/\b\w/g, l => l.toUpperCase());
  }

  /**
   * Group fields by component type
   * @param {Array} fields - Array of field objects
   * @returns {Object} Fields grouped by component
   */
  static groupFieldsByComponent(fields) {
    return {
      card: fields.filter(f => f.component === "card"),
      billing: fields.filter(f => f.component === "billing"),
      shipping: fields.filter(f => f.component === "shipping"),
      bank: fields.filter(f => f.component === "bank"),
      wallet: fields.filter(f => f.component === "wallet"),
      crypto: fields.filter(f => f.component === "crypto"),
      upi: fields.filter(f => f.component === "upi"),
      voucher: fields.filter(f => f.component === "voucher"),
      gift_card: fields.filter(f => f.component === "gift_card"),
      mobile_payment: fields.filter(f => f.component === "mobile_payment"),
      other: fields.filter(f => !["card", "billing", "shipping", "bank", "wallet", "crypto", "upi", "voucher", "gift_card", "mobile_payment"].includes(f.component))
    };
  }

  /**
   * Get active contexts from form data
   * @param {Object} context - Context object
   * @returns {Array} Array of active context objects
   */
  static getActiveContexts(context) {
    return Object.entries(context)
      .filter(([_, value]) => value)
      .map(([key, value]) => ({ key, value }));
  }

  /**
   * Validate field data
   * @param {Object} field - Field object to validate
   * @returns {boolean} True if valid
   */
  static validateField(field) {
    if (!field.name || !field.display_name || !field.type) {
      return false;
    }
    return true;
  }

  /**
   * Filter fields by criteria
   * @param {Array} fields - Array of fields
   * @param {Object} criteria - Filter criteria
   * @returns {Array} Filtered fields
   */
  static filterFields(fields, criteria = {}) {
    return fields.filter(field => {
      if (criteria.required !== undefined && field.required !== criteria.required) {
        return false;
      }
      if (criteria.component && field.component !== criteria.component) {
        return false;
      }
      if (criteria.type && field.type !== criteria.type) {
        return false;
      }
      return true;
    });
  }
}
