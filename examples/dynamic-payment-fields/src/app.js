/**
 * Main Application Controller
 * Orchestrates all components and handles application lifecycle
 */

import { PAYMENT_CONFIG } from "./config/constants.js";
import { configurationService } from "./services/configurationService.js";
import { countryStateService } from "./services/countryStateService.js";
import { FilterService } from "./services/filterService.js";
import { DOMUtils } from "./utils/domUtils.js";
import { FieldUtils } from "./utils/fieldUtils.js";
import { FormGenerator } from "./components/formGenerator.js";

export class PaymentConfigApp {
  constructor() {
    this.originalPaymentMethodOptions = "";
    this.originalPaymentMethodTypeOptions = "";
  }

  /**
   * Initialize the application
   */
  async init() {
    try {
      // Initialize services
      const [configInitialized, countryStateInitialized] = await Promise.all([
        configurationService.initialize(),
        countryStateService.initialize(),
      ]);

      if (!configInitialized) {
        DOMUtils.showErrorNotification(
          "Failed to initialize configuration service"
        );
        return;
      }

      if (!countryStateInitialized) {
        console.warn(
          "Country-state service failed to initialize, using fallback data"
        );
      }

      // Setup UI
      this.populateDropdowns();
      this.attachEventListeners();
      this.setDefaults();

      console.log("Payment Configuration App initialized successfully");
    } catch (error) {
      console.error("Error initializing app:", error);
      DOMUtils.showErrorNotification("Failed to initialize application");
    }
  }

  /**
   * Populate dropdown elements with data
   */
  populateDropdowns() {
    const connectorSelect = document.querySelector('select[name="connector"]');
    const paymentMethodSelect = document.querySelector(
      'select[name="payment_method"]'
    );
    const paymentMethodTypeSelect = document.querySelector(
      'select[name="payment_method_type"]'
    );
    const countrySelect = document.querySelector('select[name="country"]');
    const mandateSelect = document.querySelector('select[name="mandate_type"]');
    const collectBillingSelect = document.querySelector(
      'select[name="collect_billing_details_from_wallet_connector"]'
    );
    const collectShippingSelect = document.querySelector(
      'select[name="collect_shipping_details_from_wallet_connector"]'
    );

    if (
      !connectorSelect ||
      !paymentMethodSelect ||
      !paymentMethodTypeSelect ||
      !countrySelect ||
      !mandateSelect ||
      !collectBillingSelect ||
      !collectShippingSelect
    ) {
      console.error("Required dropdown elements not found");
      return;
    }

    // Populate dropdowns
    DOMUtils.populateSelect(
      connectorSelect,
      PAYMENT_CONFIG.connectors,
      "Select Connector"
    );
    DOMUtils.populateSelect(
      paymentMethodSelect,
      PAYMENT_CONFIG.paymentMethods,
      "Select payment method"
    );
    DOMUtils.populateSelect(
      paymentMethodTypeSelect,
      PAYMENT_CONFIG.paymentMethodTypes,
      "Select payment method type"
    );
    DOMUtils.populateSelect(
      countrySelect,
      PAYMENT_CONFIG.countries,
      "Select Country"
    );
    DOMUtils.populateSelect(
      mandateSelect,
      PAYMENT_CONFIG.mandateTypes,
      "Select mandate type"
    );
    DOMUtils.populateSelect(
      collectBillingSelect,
      PAYMENT_CONFIG.collectBillingDetailsFromWalletConnector,
      "Select billing collection"
    );
    DOMUtils.populateSelect(
      collectShippingSelect,
      PAYMENT_CONFIG.collectShippingDetailsFromWalletConnector,
      "Select shipping collection"
    );

    // Store original options for restoration
    this.originalPaymentMethodOptions = paymentMethodSelect.innerHTML;
    this.originalPaymentMethodTypeOptions = paymentMethodTypeSelect.innerHTML;
  }

  /**
   * Attach event listeners to form elements
   */
  attachEventListeners() {
    const connectorSelect = document.querySelector('select[name="connector"]');
    const paymentMethodSelect = document.querySelector(
      'select[name="payment_method"]'
    );
    const paymentMethodTypeSelect = document.querySelector(
      'select[name="payment_method_type"]'
    );
    const countrySelect = document.querySelector('select[name="country"]');
    const mandateSelect = document.querySelector('select[name="mandate_type"]');

    // Connector change handler
    DOMUtils.addEventListener('select[name="connector"]', "change", (e) => {
      const selectedConnector = e.target.value;
      if (selectedConnector) {
        FilterService.filterByConnector(selectedConnector);
        this.updateConfiguration();
      } else {
        FilterService.resetFilters(
          this.originalPaymentMethodOptions,
          this.originalPaymentMethodTypeOptions
        );
        this.updateConfiguration();
      }
    });

    // Payment method change handler
    DOMUtils.addEventListener(
      'select[name="payment_method"]',
      "change",
      (e) => {
        const selectedMethod = e.target.value;
        if (selectedMethod) {
          FilterService.filterPaymentMethodTypes(selectedMethod);
          this.updateConfiguration();
        } else {
          const selectedConnector = connectorSelect?.value;
          if (selectedConnector) {
            const validTypes =
              FilterService.getValidPaymentMethodTypes(selectedConnector);
            DOMUtils.populateSelect(
              paymentMethodTypeSelect,
              validTypes,
              "Select payment method type"
            );
          } else {
            FilterService.resetFilters(
              null,
              this.originalPaymentMethodTypeOptions
            );
          }
          this.updateConfiguration();
        }
      }
    );

    // Other field change handlers
    DOMUtils.addEventListener(
      'select[name="payment_method_type"]',
      "change",
      () => this.updateConfiguration()
    );
    DOMUtils.addEventListener('select[name="country"]', "change", () =>
      this.updateConfiguration()
    );
    DOMUtils.addEventListener('select[name="mandate_type"]', "change", () =>
      this.updateConfiguration()
    );
    DOMUtils.addEventListener(
      'select[name="collect_billing_details_from_wallet_connector"]',
      "change",
      () => this.updateConfiguration()
    );
    DOMUtils.addEventListener(
      'select[name="collect_shipping_details_from_wallet_connector"]',
      "change",
      () => this.updateConfiguration()
    );

    // Global form submit handler
    window.handleFormSubmit = (event) => this.handleFormSubmit(event);
    window.resetForm = () => this.resetPaymentForm();
  }

  /**
   * Set default values
   */
  setDefaults() {
    // Don't set default values initially to show welcome form
    // Values will be set when user selects sample configurations
    this.updateConfiguration();
  }

  /**
   * Update configuration based on current form state
   */
  updateConfiguration() {
    const context = this.getFormContext();
    const activeContexts = FieldUtils.getActiveContexts(context);

    // Show welcome form when no context is selected
    if (activeContexts.length === 0 || !configurationService.isInitialized()) {
      DOMUtils.updateElementContent(
        "config-table",
        FormGenerator.generateWelcomeForm()
      );
      return;
    }

    try {
      // Evaluate configuration using the service
      const resolvedConfig =
        configurationService.evaluateConfiguration(context);

      if (!resolvedConfig) {
        DOMUtils.updateElementContent(
          "config-table",
          FormGenerator.generateErrorState()
        );
        return;
      }

      // Parse configuration to fields
      const fields = FieldUtils.parseResolvedConfigToFields(resolvedConfig);

      if (fields.length === 0) {
        DOMUtils.updateElementContent(
          "config-table",
          FormGenerator.generateEmptyState(context)
        );
        return;
      }

      // Group fields and generate form
      const fieldGroups = FieldUtils.groupFieldsByComponent(fields);
      const totalRequiredFields = fields.length; // All fields are now required since we filter out non-required ones

      const formHtml = FormGenerator.generateFormHtml(
        activeContexts,
        fieldGroups,
        totalRequiredFields
      );
      DOMUtils.updateElementContent("config-table", formHtml);
    } catch (error) {
      console.error("Error updating configuration:", error);
      DOMUtils.updateElementContent(
        "config-table",
        FormGenerator.generateErrorState()
      );
    }
  }

  /**
   * Get current form context
   * @returns {Object} Context object
   */
  getFormContext() {
    return DOMUtils.getFormData("context-form");
  }

  /**
   * Handle form submission
   * @param {Event} event - Form submit event
   */
  handleFormSubmit(event) {
    event.preventDefault();

    try {
      const formData = new FormData(event.target);
      const data = Object.fromEntries(formData.entries());

      // Validate required fields
      const requiredFields = event.target.querySelectorAll("[required]");
      const missingFields = [];

      requiredFields.forEach((field) => {
        if (!field.value.trim()) {
          missingFields.push(field.name);
        }
      });

      if (missingFields.length > 0) {
        DOMUtils.showErrorNotification(
          `Please fill in required fields: ${missingFields.join(", ")}`
        );
        return;
      }

      // Show success message
      DOMUtils.showSuccessNotification(
        "Payment configuration submitted successfully!"
      );
      console.log("Form submitted with data:", data);

      // Optional: Send data to server
      // this.submitToServer(data);
    } catch (error) {
      console.error("Error submitting form:", error);
      DOMUtils.showErrorNotification(
        "Error submitting form. Please try again."
      );
    }
  }

  /**
   * Reset the payment form
   */
  resetPaymentForm() {
    const paymentForm = document.querySelector(".form-section");
    if (paymentForm) {
      paymentForm.reset();
      DOMUtils.showSuccessNotification("Form reset successfully");
    }
  }

  /**
   * Submit data to server (placeholder)
   * @param {Object} data - Form data to submit
   */
  async submitToServer(data) {
    try {
      // Placeholder for server submission
      console.log("Submitting to server:", data);

      // Example API call:
      // const response = await fetch('/api/payment-config', {
      //   method: 'POST',
      //   headers: { 'Content-Type': 'application/json' },
      //   body: JSON.stringify(data)
      // });

      // if (!response.ok) throw new Error('Server error');
    } catch (error) {
      console.error("Server submission error:", error);
      DOMUtils.showErrorNotification("Failed to submit to server");
    }
  }

  /**
   * Get application state
   * @returns {Object} Current application state
   */
  getState() {
    return {
      context: this.getFormContext(),
      isInitialized: configurationService.isInitialized(),
      timestamp: new Date().toISOString(),
    };
  }

  /**
   * Destroy the application and cleanup
   */
  destroy() {
    // Remove global handlers
    delete window.handleFormSubmit;
    delete window.resetForm;

    // Reset configuration service
    configurationService.reset();

    console.log("Payment Configuration App destroyed");
  }
}

// Global functions for backward compatibility
window.updateConfiguration = function () {
  if (window.paymentApp) {
    window.paymentApp.updateConfiguration();
  }
};

// Global function to handle country changes
window.handleCountryChange = function (selectElement) {
  const countryCode = selectElement.value;
  const form =
    selectElement.closest("form") || selectElement.closest(".form-section");

  if (!form || !countryCode) return;

  // Find related state select in the same form
  const stateSelects = form.querySelectorAll('select[name*="state"]');

  stateSelects.forEach((stateSelect) => {
    // Update state options based on selected country
    if (countryStateService.isInitialized()) {
      const states = countryStateService.getStatesByCountry(countryCode);
      const stateOptions = states
        .map(
          (state) =>
            `<option value="${state.code || state.value}">${
              state.value
            }</option>`
        )
        .join("");

      stateSelect.innerHTML = `
        <option value="">Select State</option>
        ${stateOptions}
      `;
    }
  });

  // Find related phone code selects and auto-populate
  const phoneCodeSelects = form.querySelectorAll(
    'select[name*="country_code"], select[name*="phone_code"]'
  );

  phoneCodeSelects.forEach((phoneCodeSelect) => {
    if (countryStateService.isInitialized()) {
      const phoneCode = countryStateService.getPhoneCodeByCountry(countryCode);
      if (phoneCode) {
        phoneCodeSelect.value = phoneCode;
      }
    }
  });

  // Update phone number placeholders with format examples
  const phoneInputs = form.querySelectorAll(
    'input[type="tel"], input[name*="phone"]'
  );

  phoneInputs.forEach((phoneInput) => {
    if (countryStateService.isInitialized()) {
      const formatExample =
        countryStateService.getPhoneFormatExample(countryCode);
      if (formatExample) {
        phoneInput.placeholder = formatExample;
      }
    }
  });
};

// Global function to load sample configurations
window.loadSampleConfig = function (configType) {
  if (!window.paymentApp) return;

  const sampleConfigs = {
    card: {
      connector: "Stripe",
      payment_method: "Card",
      payment_method_type: "Credit",
      country: "US",
      mandate_type: "non_mandate",
    },
    bank: {
      connector: "Adyen",
      payment_method: "BankDebit",
      payment_method_type: "Ach",
      country: "US",
      mandate_type: "mandate",
    },
    wallet: {
      connector: "Paypal",
      payment_method: "Wallet",
      payment_method_type: "Paypal",
      country: "US",
      mandate_type: "non_mandate",
    },
    crypto: {
      connector: "Cryptopay",
      payment_method: "Crypto",
      payment_method_type: "CryptoCurrency",
      country: "US",
      mandate_type: "non_mandate",
    },
  };

  const config = sampleConfigs[configType];
  if (!config) return;

  // Set form values
  const form = document.getElementById("context-form");
  if (form) {
    Object.entries(config).forEach(([key, value]) => {
      const select = form.querySelector(`select[name="${key}"]`);
      if (select) {
        select.value = value;
        // Trigger change event to update dependent dropdowns
        select.dispatchEvent(new Event("change", { bubbles: true }));
      }
    });
  }

  // Small delay to ensure all change events are processed
  setTimeout(() => {
    window.paymentApp.updateConfiguration();
  }, 100);
};

// Global function to handle cryptocurrency changes
window.handleCurrencyChange = function (selectElement) {
  const selectedCurrency = selectElement.value;
  const form =
    selectElement.closest("form") || selectElement.closest(".form-section");

  if (!form) return;

  // Find related network select in the same form
  const networkSelects = form.querySelectorAll(
    'select[name*="network"], select[name*="Network"]'
  );

  networkSelects.forEach((networkSelect) => {
    // Import the mapping dynamically
    import("./config/mappings.js")
      .then(({ CRYPTO_NETWORK_MAPPING }) => {
        if (!selectedCurrency) {
          networkSelect.innerHTML =
            '<option value="">Select Currency First</option>';
          return;
        }

        // Get available networks for the selected currency
        const availableNetworks =
          CRYPTO_NETWORK_MAPPING[selectedCurrency] || [];
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

        networkSelect.innerHTML = `
        <option value="">Select Network</option>
        ${networkOptions}
      `;

        // If only one network available, auto-select it
        if (availableNetworks.length === 1) {
          networkSelect.value = availableNetworks[0];
        }
      })
      .catch((error) => {
        console.error("Error loading crypto network mapping:", error);
      });
  });
};

// Initialize app when DOM is ready
document.addEventListener("DOMContentLoaded", async function () {
  window.paymentApp = new PaymentConfigApp();
  await window.paymentApp.init();
});
