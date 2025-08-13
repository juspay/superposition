/**
 * DOM Utility Functions
 * Helper functions for DOM manipulation and element creation
 */

export class DOMUtils {
  /**
   * Create an option element
   * @param {string} value - Option value
   * @param {string} text - Option display text
   * @returns {string} HTML string for option element
   */
  static createOption(value, text = value) {
    return `<option value="${value}">${text}</option>`;
  }

  /**
   * Populate a select element with options
   * @param {HTMLSelectElement} selectElement - The select element to populate
   * @param {Array} options - Array of option values
   * @param {string} placeholder - Placeholder text for empty option
   */
  static populateSelect(selectElement, options, placeholder = "Select option") {
    const optionsHtml = [
      this.createOption("", placeholder),
      ...options.map((option) => this.createOption(option)),
    ].join("");

    selectElement.innerHTML = optionsHtml;
  }

  /**
   * Show success notification
   * @param {string} message - Success message to display
   */
  static showSuccessNotification(message) {
    const successDiv = document.createElement("div");
    successDiv.className =
      "fixed top-4 right-4 bg-green-500 text-white px-6 py-3 rounded-lg shadow-lg z-50";
    successDiv.innerHTML = `✅ ${message}`;
    document.body.appendChild(successDiv);

    // Remove after 3 seconds
    setTimeout(() => {
      if (document.body.contains(successDiv)) {
        document.body.removeChild(successDiv);
      }
    }, 3000);
  }

  /**
   * Show error notification
   * @param {string} message - Error message to display
   */
  static showErrorNotification(message) {
    const errorDiv = document.createElement("div");
    errorDiv.className =
      "fixed top-4 right-4 bg-red-500 text-white px-6 py-3 rounded-lg shadow-lg z-50";
    errorDiv.innerHTML = `❌ ${message}`;
    document.body.appendChild(errorDiv);

    // Remove after 5 seconds
    setTimeout(() => {
      if (document.body.contains(errorDiv)) {
        document.body.removeChild(errorDiv);
      }
    }, 5000);
  }

  /**
   * Get form data as object
   * @param {string} formId - ID of the form element
   * @returns {Object} Form data as key-value pairs
   */
  static getFormData(formId) {
    const form = document.getElementById(formId);
    if (!form) return {};

    const formData = new FormData(form);
    return Object.fromEntries(formData.entries());
  }

  /**
   * Reset form by ID
   * @param {string} formId - ID of the form to reset
   */
  static resetForm(formId) {
    const form = document.getElementById(formId);
    if (form) {
      form.reset();
    }
  }

  /**
   * Update element content safely
   * @param {string} elementId - ID of element to update
   * @param {string} content - HTML content to set
   */
  static updateElementContent(elementId, content) {
    const element = document.getElementById(elementId);
    if (element) {
      element.innerHTML = content;
    }
  }

  /**
   * Add event listener with error handling
   * @param {string} selector - CSS selector for element
   * @param {string} event - Event type
   * @param {Function} handler - Event handler function
   */
  static addEventListener(selector, event, handler) {
    const element = document.querySelector(selector);
    if (element) {
      element.addEventListener(event, (e) => {
        try {
          handler(e);
        } catch (error) {
          console.error(`Error in ${event} handler for ${selector}:`, error);
          this.showErrorNotification("An error occurred. Please try again.");
        }
      });
    }
  }
}
