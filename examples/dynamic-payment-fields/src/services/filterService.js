/**
 * Filter Service
 * Handles payment method filtering based on connector selection
 */

import { CONNECTOR_PAYMENT_METHODS, CONNECTOR_PAYMENT_METHOD_TYPES, PAYMENT_METHOD_TYPES } from '../config/mappings.js';
import { DOMUtils } from '../utils/domUtils.js';

export class FilterService {
  /**
   * Filter payment methods based on selected connector
   * @param {string} selectedConnector - Selected connector name
   */
  static filterByConnector(selectedConnector) {
    const paymentMethodSelect = document.querySelector('select[name="payment_method"]');
    const paymentMethodTypeSelect = document.querySelector('select[name="payment_method_type"]');

    if (!paymentMethodSelect || !paymentMethodTypeSelect) {
      console.error('Payment method select elements not found');
      return;
    }

    const validMethods = CONNECTOR_PAYMENT_METHODS[selectedConnector] || [];
    const validTypes = CONNECTOR_PAYMENT_METHOD_TYPES[selectedConnector] || [];

    DOMUtils.populateSelect(paymentMethodSelect, validMethods, "Select payment method");
    DOMUtils.populateSelect(paymentMethodTypeSelect, validTypes, "Select payment method type");

    // Auto-select first valid options
    if (validMethods.length > 0) {
      paymentMethodSelect.value = validMethods[0];
    }
    if (validTypes.length > 0) {
      paymentMethodTypeSelect.value = validTypes[0];
    }
  }

  /**
   * Filter payment method types based on selected payment method
   * @param {string} selectedPaymentMethod - Selected payment method
   */
  static filterPaymentMethodTypes(selectedPaymentMethod) {
    const paymentMethodTypeSelect = document.querySelector('select[name="payment_method_type"]');
    const connectorSelect = document.querySelector('select[name="connector"]');
    
    if (!paymentMethodTypeSelect || !connectorSelect) {
      console.error('Required select elements not found');
      return;
    }

    const selectedConnector = connectorSelect.value;
    let validTypes = [];

    if (selectedConnector && CONNECTOR_PAYMENT_METHOD_TYPES[selectedConnector]) {
      const connectorTypes = CONNECTOR_PAYMENT_METHOD_TYPES[selectedConnector];
      
      if (selectedPaymentMethod && PAYMENT_METHOD_TYPES[selectedPaymentMethod]) {
        const methodTypes = PAYMENT_METHOD_TYPES[selectedPaymentMethod];
        validTypes = connectorTypes.filter(type => methodTypes.includes(type));
      } else {
        validTypes = connectorTypes;
      }
    } else if (selectedPaymentMethod && PAYMENT_METHOD_TYPES[selectedPaymentMethod]) {
      validTypes = PAYMENT_METHOD_TYPES[selectedPaymentMethod];
    }

    DOMUtils.populateSelect(paymentMethodTypeSelect, validTypes, "Select payment method type");
    
    if (validTypes.length > 0) {
      paymentMethodTypeSelect.value = validTypes[0];
    }
  }

  /**
   * Reset filters to original state
   * @param {string} originalPaymentMethodOptions - Original payment method options HTML
   * @param {string} originalPaymentMethodTypeOptions - Original payment method type options HTML
   */
  static resetFilters(originalPaymentMethodOptions, originalPaymentMethodTypeOptions) {
    const paymentMethodSelect = document.querySelector('select[name="payment_method"]');
    const paymentMethodTypeSelect = document.querySelector('select[name="payment_method_type"]');

    if (paymentMethodSelect && originalPaymentMethodOptions) {
      paymentMethodSelect.innerHTML = originalPaymentMethodOptions;
    }
    if (paymentMethodTypeSelect && originalPaymentMethodTypeOptions) {
      paymentMethodTypeSelect.innerHTML = originalPaymentMethodTypeOptions;
    }
  }

  /**
   * Get valid payment methods for a connector
   * @param {string} connector - Connector name
   * @returns {Array} Array of valid payment methods
   */
  static getValidPaymentMethods(connector) {
    return CONNECTOR_PAYMENT_METHODS[connector] || [];
  }

  /**
   * Get valid payment method types for a connector
   * @param {string} connector - Connector name
   * @returns {Array} Array of valid payment method types
   */
  static getValidPaymentMethodTypes(connector) {
    return CONNECTOR_PAYMENT_METHOD_TYPES[connector] || [];
  }

  /**
   * Check if a payment method is valid for a connector
   * @param {string} connector - Connector name
   * @param {string} paymentMethod - Payment method to check
   * @returns {boolean} True if valid
   */
  static isValidPaymentMethod(connector, paymentMethod) {
    const validMethods = this.getValidPaymentMethods(connector);
    return validMethods.includes(paymentMethod);
  }

  /**
   * Check if a payment method type is valid for a connector
   * @param {string} connector - Connector name
   * @param {string} paymentMethodType - Payment method type to check
   * @returns {boolean} True if valid
   */
  static isValidPaymentMethodType(connector, paymentMethodType) {
    const validTypes = this.getValidPaymentMethodTypes(connector);
    return validTypes.includes(paymentMethodType);
  }
}
