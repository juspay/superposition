/**
 * Form Generator Component
 * Handles generation of the complete payment form
 */

import { COMPONENT_CONFIG } from "../config/constants.js";
import { LayoutRenderers } from "./layoutRenderers.js";
import { FieldRenderer } from "./fieldRenderer.js";

export class FormGenerator {
  /**
   * Generate complete form HTML
   * @param {Array} activeContexts - Array of active context objects
   * @param {Object} fieldGroups - Fields grouped by component
   * @param {number} totalFields - Total number of required fields
   * @returns {string} Complete form HTML
   */
  static generateFormHtml(activeContexts, fieldGroups, totalFields) {
    const renderComponentSection = (componentKey, fields) => {
      if (!fields || fields.length === 0) return "";

      let fieldsHtml = "";

      // Use specialized rendering methods for different components
      switch (componentKey) {
        case "card":
          fieldsHtml = LayoutRenderers.renderCardFields(fields);
          break;
        case "billing":
        case "shipping":
          fieldsHtml = LayoutRenderers.renderAddressFields(
            fields,
            componentKey === "shipping"
          );
          break;
        case "bank":
          fieldsHtml = LayoutRenderers.renderBankFields(fields);
          break;
        case "crypto":
          fieldsHtml = LayoutRenderers.renderCryptoFields(fields);
          break;
        default:
          fieldsHtml = FieldRenderer.renderGenericFields(fields);
          break;
      }

      return `
        <div class="component-section mb-6 p-4 bg-white border border-gray-200 rounded-lg shadow-sm">
          <div class="flex items-center mb-4 pb-2 border-b border-gray-100">
            <span class="text-lg mr-2">${COMPONENT_CONFIG.icons[componentKey]}</span>
            <h4 class="text-lg font-semibold text-gray-900">${COMPONENT_CONFIG.names[componentKey]}</h4>
            <span class="ml-auto text-xs bg-gray-100 text-gray-600 px-2 py-1 rounded-full">${fields.length} fields</span>
          </div>
          <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
            ${fieldsHtml}
          </div>
        </div>
      `;
    };

    return `
      <div class="payment-form p-6">
        <div class="text-center mb-6">
          <h2 class="text-2xl font-bold text-gray-900 mb-2">Payment Configuration Form</h2>
          <p class="text-gray-600">Configure payment fields based on your selected context</p>
        </div>

        <div class="mb-6 p-4 bg-gradient-to-r from-blue-50 to-indigo-50 rounded-lg border border-blue-200">
          <h3 class="text-lg font-semibold text-blue-900 mb-3 flex items-center">
            <svg class="w-5 h-5 mr-2" fill="currentColor" viewBox="0 0 20 20">
              <path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z" clip-rule="evenodd"></path>
            </svg>
            Active Context
          </h3>
          <div class="flex flex-wrap">
            ${activeContexts
              .map(
                ({ key, value }) =>
                  `<span class="context-badge">${COMPONENT_CONFIG.icons[key]} ${value}</span>`
              )
              .join("")}
          </div>
        </div>

        <form class="form-section" onsubmit="handleFormSubmit(event)">
          <div class="mb-6">
            <h3 class="text-lg font-semibold text-gray-900 mb-4 flex items-center">
              <svg class="w-5 h-5 mr-2 text-green-600" fill="currentColor" viewBox="0 0 20 20">
                <path fill-rule="evenodd" d="M4 4a2 2 0 00-2 2v4a2 2 0 002 2V6h10a2 2 0 00-2-2H4zm2 6a2 2 0 012-2h8a2 2 0 012 2v4a2 2 0 01-2 2H8a2 2 0 01-2-2v-4zm6 4a2 2 0 100-4 2 2 0 000 4z" clip-rule="evenodd"></path>
              </svg>
              Payment Information (${totalFields} fields)
            </h3>

            ${this.renderSectionsInOrder(fieldGroups, renderComponentSection)}
          </div>

          <div class="flex justify-between items-center pt-4 border-t border-gray-200">
            <div class="text-sm text-gray-600">
              <span class="text-red-500">*</span> Required fields
            </div>
            <div class="space-x-3">
              <button type="button" onclick="resetForm()" class="px-4 py-2 text-sm font-medium text-gray-700 bg-white border border-gray-300 rounded-md hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500">
                Reset
              </button>
              <button type="submit" class="px-6 py-2 text-sm font-medium text-white bg-indigo-600 border border-transparent rounded-md hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500">
                Submit Payment
              </button>
            </div>
          </div>
        </form>

        <div class="mt-4 p-3 bg-gray-50 rounded-lg">
          <p class="text-xs text-gray-600">
            💡 This form is dynamically generated based on your connector and context selection. 
            Fields marked with <span class="text-red-500">*</span> are required for this configuration.
          </p>
        </div>
      </div>
    `;
  }

  /**
   * Render sections in the specified order
   * @param {Object} fieldGroups - Fields grouped by component
   * @param {Function} renderComponentSection - Function to render each section
   * @returns {string} HTML for all sections
   */
  static renderSectionsInOrder(fieldGroups, renderComponentSection) {
    // Define the order: Card first, then other payment types, then billing/shipping
    const sectionOrder = [
      // Card first
      ["card", fieldGroups.card],
      // Other payment method sections
      ["crypto", fieldGroups.crypto],
      ["wallet", fieldGroups.wallet],
      ["upi", fieldGroups.upi],
      ["voucher", fieldGroups.voucher],
      ["gift_card", fieldGroups.gift_card],
      ["mobile_payment", fieldGroups.mobile_payment],
      ["bank", fieldGroups.bank],
      ["other", fieldGroups.other],
      // Billing and Shipping last
      ["billing", fieldGroups.billing],
      ["shipping", fieldGroups.shipping],
    ];

    return sectionOrder
      .map(([componentKey, fields]) =>
        renderComponentSection(componentKey, fields)
      )
      .join("");
  }

  /**
   * Generate welcome form when nothing is selected
   * @returns {string} Welcome form HTML
   */
  static generateWelcomeForm() {
    return `
      <div class="payment-form p-6">
        <div class="text-center mb-8 p-8">
          <div class="mx-auto flex items-center justify-center h-16 w-16 rounded-full bg-gradient-to-r from-blue-500 to-indigo-600 mb-4">
            <svg class="h-8 w-8 text-white" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 10V3L4 14h7v7l9-11h-7z" />
            </svg>
          </div>
          <h2 class="text-3xl font-bold text-gray-900 mb-2">Welcome to Superposition Demo</h2>
          <p class="text-lg text-gray-600 max-w-2xl mx-auto">
            Experience dynamic payment configuration powered by Superposition. 
            Select your payment processing context to generate the appropriate form fields.
          </p>
        </div>

          <!-- Getting Started -->
          <div class="bg-gradient-to-r from-blue-50 to-indigo-50 rounded-lg p-6 border border-blue-200">
            <h3 class="text-xl font-semibold text-blue-900 mb-4 flex items-center">
              <svg class="w-6 h-6 mr-2" fill="currentColor" viewBox="0 0 20 20">
                <path fill-rule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clip-rule="evenodd"></path>
              </svg>
              Getting Started
            </h3>
            <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
              <div>
                <h4 class="font-medium text-blue-900 mb-2">1. Select Configuration Context</h4>
                <p class="text-blue-700 text-sm mb-4">
                  Use the sidebar to select your connector, payment method, type, country, and mandate preferences.
                </p>
                
                <h4 class="font-medium text-blue-900 mb-2">2. View Dynamic Form</h4>
                <p class="text-blue-700 text-sm">
                  Watch as the form automatically generates the appropriate fields based on your selections.
                </p>
              </div>
              <div>
                <h4 class="font-medium text-blue-900 mb-2">3. Test Functionality</h4>
                <p class="text-blue-700 text-sm mb-4">
                  Interact with country-state dropdowns, phone validation, and other dynamic features.
                </p>
                
                <h4 class="font-medium text-blue-900 mb-2">4. Submit Configuration</h4>
                <p class="text-blue-700 text-sm">
                  Fill out the generated form and submit to see the structured payment data output.
                </p>
              </div>
            </div>
          </div>

          <!-- Sample Configurations -->
          <div class="mt-8">
            <h3 class="text-xl font-semibold text-gray-900 mb-4">Try These Sample Configurations</h3>
            <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
              <button onclick="loadSampleConfig('card')" class="text-left p-4 bg-white border border-gray-200 rounded-lg hover:border-blue-300 hover:shadow-sm transition-all">
                <div class="flex items-center mb-2">
                  <span class="text-lg mr-2">💳</span>
                  <h4 class="font-medium text-gray-900">Card Payment</h4>
                </div>
                <p class="text-sm text-gray-600">Credit/debit card processing with billing address</p>
              </button>
              
              <button onclick="loadSampleConfig('bank')" class="text-left p-4 bg-white border border-gray-200 rounded-lg hover:border-blue-300 hover:shadow-sm transition-all">
                <div class="flex items-center mb-2">
                  <span class="text-lg mr-2">🏦</span>
                  <h4 class="font-medium text-gray-900">Bank Transfer</h4>
                </div>
                <p class="text-sm text-gray-600">Direct bank transfer with account details</p>
              </button>
              
              <button onclick="loadSampleConfig('wallet')" class="text-left p-4 bg-white border border-gray-200 rounded-lg hover:border-blue-300 hover:shadow-sm transition-all">
                <div class="flex items-center mb-2">
                  <span class="text-lg mr-2">👛</span>
                  <h4 class="font-medium text-gray-900">Digital Wallet</h4>
                </div>
                <p class="text-sm text-gray-600">Digital wallet integration with preferences</p>
              </button>
              
              <button onclick="loadSampleConfig('crypto')" class="text-left p-4 bg-white border border-gray-200 rounded-lg hover:border-blue-300 hover:shadow-sm transition-all">
                <div class="flex items-center mb-2">
                  <span class="text-lg mr-2">₿</span>
                  <h4 class="font-medium text-gray-900">Cryptocurrency</h4>
                </div>
                <p class="text-sm text-gray-600">Crypto payments with network selection</p>
              </button>
            </div>
          </div>
        </div>
      </div>
    `;
  }

  /**
   * Generate empty state HTML
   * @param {Object} context - Current context
   * @returns {string} Empty state HTML
   */
  static generateEmptyState(context) {
    return `
      <div class="p-6 text-center">
        <div class="mx-auto flex items-center justify-center h-12 w-12 rounded-full bg-yellow-100 mb-4">
          <svg class="h-6 w-6 text-yellow-600" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-2.5L13.732 4c-.77-.833-1.732-.833-2.5 0L4.268 18.5c-.77.833.192 2.5 1.732 2.5z" />
          </svg>
        </div>
        <h3 class="text-lg font-medium text-gray-900 mb-2">No Configuration Found</h3>
        <p class="text-yellow-600 mb-4">No matching configuration found for the selected context.</p>
        <div class="bg-gray-50 rounded-lg p-3">
          <p class="text-sm text-gray-500">Current Context:</p>
          <code class="text-xs text-gray-700">${JSON.stringify(
            context,
            null,
            2
          )}</code>
        </div>
      </div>
    `;
  }

  /**
   * Generate error state HTML
   * @param {string} errorMessage - Error message to display
   * @returns {string} Error state HTML
   */
  static generateErrorState(
    errorMessage = "Error loading configuration. Please try again."
  ) {
    return `
      <div class="p-6 text-center">
        <div class="mx-auto flex items-center justify-center h-12 w-12 rounded-full bg-red-100 mb-4">
          <svg class="h-6 w-6 text-red-600" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
          </svg>
        </div>
        <h3 class="text-lg font-medium text-gray-900 mb-2">Configuration Error</h3>
        <p class="text-red-600">${errorMessage}</p>
      </div>
    `;
  }
}
