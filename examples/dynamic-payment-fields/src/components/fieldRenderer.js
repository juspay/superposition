/**
 * Field Renderer Component
 * Handles rendering of different field types and layouts
 */

import { CSS_CLASSES } from "../config/constants.js";
import { countryStateService } from "../services/countryStateService.js";
import { CRYPTO_NETWORK_MAPPING } from "../config/mappings.js";

export class FieldRenderer {
  /**
   * Wrap select element with custom dropdown styling
   * @param {string} selectHtml - The select element HTML
   * @returns {string} Wrapped select HTML
   */
  static wrapSelectWithDropdownIcon(selectHtml) {
    return `
      <div class="relative">
        ${selectHtml}
        <div class="self-center absolute pointer-events-none" style="color: rgb(84, 84, 84); left: 97%; margin-left: -1rem; top: 42%;">
          <svg class="fill-current" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512" width="10" height="10">
            <path d="M12.08 70.78c-16.17-16.24-16.09-42.54.15-58.7 16.25-16.17 42.54-16.09 58.71.15L256 197.76 441.06 12.23c16.17-16.24 42.46-16.32 58.71-.15 16.24 16.16 16.32 42.46.15 58.7L285.27 285.96c-16.24 16.17-42.54 16.09-58.7-.15L12.08 70.78z"/>
          </svg>
        </div>
      </div>
    `;
  }

  /**
   * Render a single field
   * @param {Object} field - Field object
   * @param {string} customClasses - Additional CSS classes
   * @returns {string} HTML string for the field
   */
  static renderField(field, customClasses = "") {
    const required = field.required ? "required" : "";
    const requiredMark = field.required
      ? '<span class="text-red-500 ml-1">*</span>'
      : "";
    const fieldClasses = customClasses || "mb-4";

    const inputHtml = this.generateInputHtml(field, required);

    return `
      <div class="${fieldClasses}">
        <label class="block mb-2 text-sm font-medium text-gray-900">
          ${field.display_name}${requiredMark}
          <span class="text-xs text-gray-500 ml-2">(${
            field.type == "country_code_select" ? "code" : field.type
          } field)</span>
        </label>
        ${inputHtml}
        <p class="mt-1 text-xs text-gray-500 break-all">Output: ${
          field.output_path
        }</p>
      </div>
    `;
  }

  /**
   * Generate input HTML based on field type
   * @param {Object} field - Field object
   * @param {string} required - Required attribute string
   * @returns {string} HTML string for the input
   */
  static generateInputHtml(field, required) {
    const baseClasses = CSS_CLASSES.input;

    switch (field.type) {
      case "text_input":
        return `<input type="text" name="${field.name}" class="${baseClasses}" placeholder="${field.display_name}" ${required} />`;

      case "email_input":
        return `<input type="email" name="${field.name}" class="${baseClasses}" placeholder="${field.display_name}" ${required} />`;

      case "password_input":
        return `<input type="password" name="${field.name}" class="${baseClasses}" placeholder="${field.display_name}" ${required} />`;

      case "phone_input":
        return `<input type="tel" name="${field.name}" class="${baseClasses}" placeholder="${field.display_name}" ${required} />`;

      case "date_picker":
        return `<input type="date" name="${field.name}" class="${baseClasses}" ${required} />`;

      case "month_select":
        return this.generateMonthSelect(field.name, baseClasses, required);

      case "year_select":
        return this.generateYearSelect(field.name, baseClasses, required);

      case "dropdown_select":
        return this.generateDropdownSelect(field, baseClasses, required);

      case "currency_select":
        return this.generateCurrencySelect(field, baseClasses, required);

      case "country_select":
        return this.generateCountrySelect(field.name, baseClasses, required);

      case "country_code_select":
        return this.generateCountryCodeSelect(
          field.name,
          baseClasses,
          required
        );

      case "state_select":
        return this.generateStateSelect(
          field.name,
          baseClasses,
          required,
          field.countryCode
        );

      case "network_select":
        return this.generateNetworkSelect(
          field.name,
          baseClasses,
          required,
          field.selectedCurrency
        );

      default:
        return `<input type="text" name="${field.name}" class="${baseClasses}" placeholder="${field.display_name}" ${required} />`;
    }
  }

  /**
   * Generate month select dropdown
   * @param {string} name - Field name
   * @param {string} classes - CSS classes
   * @param {string} required - Required attribute
   * @returns {string} HTML string
   */
  static generateMonthSelect(name, classes, required) {
    const monthOptions = Array.from({ length: 12 }, (_, i) => {
      const month = (i + 1).toString().padStart(2, "0");
      return `<option value="${month}">${month}</option>`;
    }).join("");

    const selectHtml = `<select name="${name}" class="${classes}" ${required}>
      <option value="">Month</option>
      ${monthOptions}
    </select>`;

    return this.wrapSelectWithDropdownIcon(selectHtml);
  }

  /**
   * Generate year select dropdown
   * @param {string} name - Field name
   * @param {string} classes - CSS classes
   * @param {string} required - Required attribute
   * @returns {string} HTML string
   */
  static generateYearSelect(name, classes, required) {
    const currentYear = new Date().getFullYear();
    const yearOptions = Array.from({ length: 20 }, (_, i) => {
      const year = currentYear + i;
      return `<option value="${year}">${year}</option>`;
    }).join("");

    const selectHtml = `<select name="${name}" class="${classes}" ${required}>
      <option value="">Year</option>
      ${yearOptions}
    </select>`;

    return this.wrapSelectWithDropdownIcon(selectHtml);
  }

  /**
   * Generate dropdown select
   * @param {Object} field - Field object
   * @param {string} classes - CSS classes
   * @param {string} required - Required attribute
   * @returns {string} HTML string
   */
  static generateDropdownSelect(field, classes, required) {
    const options = field.options || [];
    const dropdownOptions = options
      .map((option) => `<option value="${option}">${option}</option>`)
      .join("");

    const selectHtml = `<select name="${field.name}" class="${classes}" ${required}>
      <option value="">Select ${field.display_name}</option>
      ${dropdownOptions}
    </select>`;

    return this.wrapSelectWithDropdownIcon(selectHtml);
  }

  /**
   * Generate country select dropdown
   * @param {string} name - Field name
   * @param {string} classes - CSS classes
   * @param {string} required - Required attribute
   * @returns {string} HTML string
   */
  static generateCountrySelect(name, classes, required) {
    if (!countryStateService.isInitialized()) {
      // Fallback if service not initialized
      const selectHtml = `<select name="${name}" class="${classes}" ${required}>
        <option value="">Select Country</option>
        <option value="US">United States</option>
        <option value="GB">United Kingdom</option>
        <option value="DE">Germany</option>
        <option value="FR">France</option>
        <option value="IN">India</option>
      </select>`;
      return this.wrapSelectWithDropdownIcon(selectHtml);
    }

    const countries = countryStateService.getCountriesSorted();
    const countryOptions = countries
      .map(
        (country) => `<option value="${country.code}">${country.name}</option>`
      )
      .join("");

    const selectHtml = `<select name="${name}" class="${classes}" ${required} onchange="handleCountryChange(this)">
      <option value="">Select Country</option>
      ${countryOptions}
    </select>`;

    return this.wrapSelectWithDropdownIcon(selectHtml);
  }

  /**
   * Generate country code select dropdown
   * @param {string} name - Field name
   * @param {string} classes - CSS classes
   * @param {string} required - Required attribute
   * @returns {string} HTML string
   */
  static generateCountryCodeSelect(name, classes, required) {
    if (!countryStateService.isInitialized()) {
      // Fallback if service not initialized
      const selectHtml = `<select name="${name}" class="${classes}" ${required}>
        <option value="">Code</option>
        <option value="+1">+1 (US/CA)</option>
        <option value="+44">+44 (UK)</option>
        <option value="+49">+49 (DE)</option>
        <option value="+33">+33 (FR)</option>
        <option value="+91">+91 (IN)</option>
      </select>`;
      return this.wrapSelectWithDropdownIcon(selectHtml);
    }

    const phoneCodes = countryStateService.getPhoneCodes();
    const phoneCodeOptions = phoneCodes
      .map((code) => {
        // Find a country that uses this code for display
        const country = countryStateService
          .getCountries()
          .find((c) => c.phoneCode === code);
        const displayText = country ? `${code} (${country.name})` : code;
        return `<option value="${code}">${displayText}</option>`;
      })
      .join("");

    const selectHtml = `<select name="${name}" class="${classes}" ${required}>
      <option value="">Select Code</option>
      ${phoneCodeOptions}
    </select>`;

    return this.wrapSelectWithDropdownIcon(selectHtml);
  }

  /**
   * Generate state select dropdown
   * @param {string} name - Field name
   * @param {string} classes - CSS classes
   * @param {string} required - Required attribute
   * @param {string} countryCode - Country code to get states for
   * @returns {string} HTML string
   */
  static generateStateSelect(name, classes, required, countryCode = "") {
    const stateSelectId = `state-${name.replace(/\./g, "-")}`;

    if (!countryStateService.isInitialized()) {
      const selectHtml = `<select name="${name}" id="${stateSelectId}" class="${classes}" ${required}>
        <option value="">Select State</option>
      </select>`;
      return this.wrapSelectWithDropdownIcon(selectHtml);
    }

    // If no country code provided, start with empty state dropdown
    if (!countryCode) {
      const selectHtml = `<select name="${name}" id="${stateSelectId}" class="${classes}" ${required}>
        <option value="">Select Country First</option>
      </select>`;
      return this.wrapSelectWithDropdownIcon(selectHtml);
    }

    const states = countryStateService.getStatesByCountry(countryCode);
    const stateOptions = states
      .map(
        (state) =>
          `<option value="${state.code || state.value}">${state.value}</option>`
      )
      .join("");

    const selectHtml = `<select name="${name}" id="${stateSelectId}" class="${classes}" ${required}>
      <option value="">Select State</option>
      ${stateOptions}
    </select>`;

    return this.wrapSelectWithDropdownIcon(selectHtml);
  }

  /**
   * Generate cryptocurrency select dropdown with network integration
   * @param {Object} field - Field object
   * @param {string} classes - CSS classes
   * @param {string} required - Required attribute
   * @returns {string} HTML string
   */
  static generateCurrencySelect(field, classes, required) {
    const options = field.options || [];
    const currencyOptions = options
      .map((option) => `<option value="${option}">${option}</option>`)
      .join("");

    const selectHtml = `<select name="${field.name}" class="${classes}" ${required} onchange="handleCurrencyChange(this)">
      <option value="">Select ${field.display_name}</option>
      ${currencyOptions}
    </select>`;

    return this.wrapSelectWithDropdownIcon(selectHtml);
  }

  /**
   * Generate network select dropdown based on selected cryptocurrency
   * @param {string} name - Field name
   * @param {string} classes - CSS classes
   * @param {string} required - Required attribute
   * @param {string} selectedCurrency - Selected cryptocurrency
   * @returns {string} HTML string
   */
  static generateNetworkSelect(name, classes, required, selectedCurrency = "") {
    const networkSelectId = `network-${name.replace(/\./g, "-")}`;

    // If no currency selected, show placeholder
    if (!selectedCurrency) {
      const selectHtml = `<select name="${name}" id="${networkSelectId}" class="${classes}" ${required}>
        <option value="">Select Currency First</option>
      </select>`;
      return this.wrapSelectWithDropdownIcon(selectHtml);
    }

    // Get available networks for the selected currency
    const availableNetworks = CRYPTO_NETWORK_MAPPING[selectedCurrency] || [];
    const networkOptions = availableNetworks
      .map((network) => {
        // Format network name for display
        const displayName = network
          .split("_")
          .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
          .join(" ");
        return `<option value="${network}">${displayName}</option>`;
      })
      .join("");

    const selectHtml = `<select name="${name}" id="${networkSelectId}" class="${classes}" ${required}>
      <option value="">Select Network</option>
      ${networkOptions}
    </select>`;

    return this.wrapSelectWithDropdownIcon(selectHtml);
  }

  /**
   * Render generic fields
   * @param {Array} fields - Array of field objects
   * @returns {string} HTML string for all fields
   */
  static renderGenericFields(fields) {
    if (!fields || fields.length === 0) return "";
    return fields.map((field) => this.renderField(field, "mb-4")).join("");
  }
}
