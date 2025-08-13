/**
 * Layout Renderers
 * Specialized renderers for different field layouts (card, address, bank)
 */

import { FieldRenderer } from "./fieldRenderer.js";

export class LayoutRenderers {
  /**
   * Render card fields with specialized layout
   * @param {Array} fields - Array of card field objects
   * @returns {string} HTML string for card fields
   */
  static renderCardFields(fields) {
    if (!fields || fields.length === 0) return "";

    // Group card fields by type for better layout
    const cardNumber = fields.find((f) => f.name.includes("card_number"));
    const cardCvc = fields.find((f) => f.name.includes("card_cvc"));
    const cardExpMonth = fields.find((f) => f.name.includes("card_exp_month"));
    const cardExpYear = fields.find((f) => f.name.includes("card_exp_year"));
    const cardNetwork = fields.find((f) => f.name.includes("card_network"));
    const otherCardFields = fields.filter(
      (f) =>
        !f.name.includes("card_number") &&
        !f.name.includes("card_cvc") &&
        !f.name.includes("card_exp_month") &&
        !f.name.includes("card_exp_year") &&
        !f.name.includes("card_network")
    );

    let html = "";

    // Card Number - Full width
    if (cardNumber) {
      html += FieldRenderer.renderField(cardNumber, "mb-4 col-span-2");
    }

    // Card Expiry (Combined Month/Year) and CVC row
    if (cardExpMonth || cardExpYear || cardCvc) {
      html += '<div class="mb-4 col-span-2 grid grid-cols-2 gap-4">';

      // Combined Expiry section (half width)
      if (cardExpMonth || cardExpYear) {
        html += "<div>";
        html += '<label class="block mb-2 text-sm font-medium text-gray-900">';
        html += "Card Expiry";
        html +=
          '<span class="text-xs text-gray-500 ml-2">(combined field)</span>';
        html += "</label>";
        html +=
          '<input type="text" placeholder="MM/YY" class="bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5" />';
        html +=
          '<p class="mt-1 text-xs text-gray-500 break-all">Output: payment_method_data.card.card_exp_month + card_exp_year</p>';
        html += "</div>";
      }

      // CVC section (half width)
      if (cardCvc) {
        html += FieldRenderer.renderField(cardCvc, "");
      }

      html += "</div>";
    }

    // Card Network - Full width if present
    if (cardNetwork) {
      html += FieldRenderer.renderField(cardNetwork, "mb-4 col-span-2");
    }

    // Other card fields
    otherCardFields.forEach((field) => {
      html += FieldRenderer.renderField(field, "mb-4");
    });

    return html;
  }

  /**
   * Render address fields with specialized layout
   * @param {Array} fields - Array of address field objects
   * @param {boolean} isShipping - Whether this is shipping address
   * @returns {string} HTML string for address fields
   */
  static renderAddressFields(fields, isShipping = false) {
    if (!fields || fields.length === 0) return "";

    // Group address fields for better layout
    const firstName = fields.find((f) => f.name.includes("first_name"));
    const lastName = fields.find((f) => f.name.includes("last_name"));
    const line1 = fields.find((f) => f.name.includes("line1"));
    const line2 = fields.find((f) => f.name.includes("line2"));
    const city = fields.find((f) => f.name.includes("city"));
    const state = fields.find((f) => f.name.includes("state"));
    const zip = fields.find((f) => f.name.includes("zip"));
    const country = fields.find((f) => f.name.includes("country"));
    const email = fields.find((f) => f.name.includes("email"));
    const phoneCode = fields.find((f) => f.name.includes("country_code"));
    const phoneNumber = fields.find(
      (f) => f.name.includes("phone") && f.name.includes("number")
    );
    const otherFields = fields.filter(
      (f) =>
        !f.name.includes("first_name") &&
        !f.name.includes("last_name") &&
        !f.name.includes("line1") &&
        !f.name.includes("line2") &&
        !f.name.includes("city") &&
        !f.name.includes("state") &&
        !f.name.includes("zip") &&
        !f.name.includes("country") &&
        !f.name.includes("email") &&
        !f.name.includes("phone")
    );

    let html = "";

    // Combined Name field - Full width
    if (firstName || lastName) {
      html += '<div class="mb-4 col-span-2">';
      html += '<label class="block mb-2 text-sm font-medium text-gray-900">';
      html += isShipping ? "Full Name" : "Cardholder Name";
      html +=
        '<span class="text-xs text-gray-500 ml-2">(combined field)</span>';
      html += "</label>";
      html +=
        '<input type="text" placeholder="First Last" class="bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5" />';
      html += '<p class="mt-1 text-xs text-gray-500 break-all">Output: ';
      if (firstName && lastName) {
        html += `${firstName.output_path} + ${lastName.output_path}`;
      } else if (firstName) {
        html += firstName.output_path;
      } else if (lastName) {
        html += lastName.output_path;
      }
      html += "</p>";
      html += "</div>";
    }

    // Address Line 1 and Line 2 - Half and Half
    if (line1 || line2) {
      html += '<div class="mb-4 col-span-2 grid grid-cols-2 gap-4">';
      if (line1) {
        html += FieldRenderer.renderField(line1, "");
      }
      if (line2) {
        html += FieldRenderer.renderField(line2, "");
      }
      html += "</div>";
    }

    // City and State - Side by side
    if (city || state) {
      html += '<div class="mb-4 col-span-2 grid grid-cols-2 gap-4">';
      if (city) {
        html += FieldRenderer.renderField(city, "");
      }
      if (state) {
        html += FieldRenderer.renderField(state, "");
      }
      html += "</div>";
    }

    // Country and ZIP - Side by side
    if (country || zip) {
      html += '<div class="mb-4 col-span-2 grid grid-cols-2 gap-4">';
      if (country) {
        html += FieldRenderer.renderField(country, "");
      }
      if (zip) {
        html += FieldRenderer.renderField(zip, "");
      }
      html += "</div>";
    }

    // Email - Full width
    if (email) {
      html += FieldRenderer.renderField(email, "mb-4 col-span-2");
    }

    // Phone - Country code and number side by side
    if (phoneCode || phoneNumber) {
      html += '<div class="mb-4 col-span-2 grid grid-cols-4 gap-4">';
      if (phoneCode) {
        html += FieldRenderer.renderField(phoneCode, "");
      }
      if (phoneNumber) {
        html += FieldRenderer.renderField(phoneNumber, "col-span-3");
      }
      html += "</div>";
    }

    // Other fields
    otherFields.forEach((field) => {
      html += FieldRenderer.renderField(field, "mb-4");
    });

    return html;
  }

  /**
   * Render bank fields with specialized layout
   * @param {Array} fields - Array of bank field objects
   * @returns {string} HTML string for bank fields
   */
  static renderBankFields(fields) {
    if (!fields || fields.length === 0) return "";

    // Group bank fields by type
    const accountFields = fields.filter((f) =>
      f.name.includes("account_number")
    );
    const routingFields = fields.filter(
      (f) =>
        f.name.includes("routing_number") ||
        f.name.includes("sort_code") ||
        f.name.includes("bsb_number")
    );
    const ibanFields = fields.filter((f) => f.name.includes("iban"));
    const bankNameFields = fields.filter(
      (f) => f.name.includes("bank_name") || f.name.includes("issuer")
    );
    const otherFields = fields.filter(
      (f) =>
        !f.name.includes("account_number") &&
        !f.name.includes("routing_number") &&
        !f.name.includes("sort_code") &&
        !f.name.includes("bsb_number") &&
        !f.name.includes("iban") &&
        !f.name.includes("bank_name") &&
        !f.name.includes("issuer")
    );

    let html = "";

    // Account and Routing fields side by side
    const maxPairs = Math.max(accountFields.length, routingFields.length);
    for (let i = 0; i < maxPairs; i++) {
      if (accountFields[i] || routingFields[i]) {
        html += '<div class="mb-4 col-span-2 grid grid-cols-2 gap-4">';
        if (accountFields[i]) {
          html += FieldRenderer.renderField(accountFields[i], "");
        }
        if (routingFields[i]) {
          html += FieldRenderer.renderField(routingFields[i], "");
        }
        html += "</div>";
      }
    }

    // IBAN fields - Full width
    ibanFields.forEach((field) => {
      html += FieldRenderer.renderField(field, "mb-4 col-span-2");
    });

    // Bank name/issuer fields - Full width
    bankNameFields.forEach((field) => {
      html += FieldRenderer.renderField(field, "mb-4 col-span-2");
    });

    // Other fields
    otherFields.forEach((field) => {
      html += FieldRenderer.renderField(field, "mb-4");
    });

    return html;
  }

  /**
   * Render crypto fields with specialized layout (currency first, then network)
   * @param {Array} fields - Array of crypto field objects
   * @returns {string} HTML string for crypto fields
   */
  static renderCryptoFields(fields) {
    if (!fields || fields.length === 0) return "";

    // Group crypto fields by type - CURRENCY FIRST, THEN NETWORK
    const currencyFields = fields.filter(
      (f) =>
        f.name.includes("currency") ||
        f.name.includes("coin") ||
        f.type === "currency_select"
    );
    const networkFields = fields.filter(
      (f) =>
        f.name.includes("network") ||
        f.name.includes("chain") ||
        f.type === "network_select"
    );
    const addressFields = fields.filter(
      (f) => f.name.includes("address") || f.name.includes("wallet")
    );
    const otherFields = fields.filter(
      (f) =>
        !f.name.includes("currency") &&
        !f.name.includes("coin") &&
        !f.name.includes("network") &&
        !f.name.includes("chain") &&
        !f.name.includes("address") &&
        !f.name.includes("wallet") &&
        f.type !== "currency_select" &&
        f.type !== "network_select"
    );

    let html = "";

    // Currency and Network fields side by side - CURRENCY FIRST
    const maxPairs = Math.max(currencyFields.length, networkFields.length);
    for (let i = 0; i < maxPairs; i++) {
      if (currencyFields[i] || networkFields[i]) {
        html += '<div class="mb-4 col-span-2 grid grid-cols-2 gap-4">';

        // CURRENCY FIRST (left side)
        if (currencyFields[i]) {
          html += FieldRenderer.renderField(currencyFields[i], "");
        } else {
          html += "<div></div>"; // Empty placeholder
        }

        // NETWORK SECOND (right side)
        if (networkFields[i]) {
          html += FieldRenderer.renderField(networkFields[i], "");
        } else {
          html += "<div></div>"; // Empty placeholder
        }

        html += "</div>";
      }
    }

    // Address/Wallet fields - Full width
    addressFields.forEach((field) => {
      html += FieldRenderer.renderField(field, "mb-4 col-span-2");
    });

    // Other crypto fields
    otherFields.forEach((field) => {
      html += FieldRenderer.renderField(field, "mb-4");
    });

    return html;
  }
}
