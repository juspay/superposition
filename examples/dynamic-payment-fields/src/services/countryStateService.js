/**
 * Country State Service
 * Handles loading and managing country, state, and phone code data
 */

export class CountryStateService {
  constructor() {
    this.data = null;
    this.countries = [];
    this.states = {};
    this.phoneCodes = [];
  }

  /**
   * Initialize the service by loading country-state data
   * @returns {Promise<boolean>} Success status
   */
  async initialize() {
    try {
      const response = await fetch('countrystate.json');
      this.data = await response.json();
      
      // Process countries
      this.countries = this.data.country.map(country => ({
        code: country.country_code,
        name: country.country_name,
        phoneCode: country.phone_number_code,
        validationRegex: country.validation_regex,
        formatExample: country.format_example,
        formatRegex: country.format_regex,
        timeZones: country.timeZones
      }));

      // Process states
      this.states = this.data.states || {};

      // Process phone codes (unique list)
      this.phoneCodes = [...new Set(this.countries.map(c => c.phoneCode))].sort();

      console.log('CountryStateService initialized successfully');
      console.log(`Loaded ${this.countries.length} countries and ${Object.keys(this.states).length} country-state mappings`);
      
      return true;
    } catch (error) {
      console.error('Error loading country-state data:', error);
      return false;
    }
  }

  /**
   * Get all countries
   * @returns {Array} Array of country objects
   */
  getCountries() {
    return this.countries;
  }

  /**
   * Get country by code
   * @param {string} countryCode - Country code (e.g., 'US', 'IN')
   * @returns {Object|null} Country object or null if not found
   */
  getCountryByCode(countryCode) {
    return this.countries.find(country => country.code === countryCode) || null;
  }

  /**
   * Get states for a country
   * @param {string} countryCode - Country code
   * @returns {Array} Array of state objects
   */
  getStatesByCountry(countryCode) {
    return this.states[countryCode] || [];
  }

  /**
   * Get all phone codes
   * @returns {Array} Array of phone codes
   */
  getPhoneCodes() {
    return this.phoneCodes;
  }

  /**
   * Get phone code for a country
   * @param {string} countryCode - Country code
   * @returns {string|null} Phone code or null if not found
   */
  getPhoneCodeByCountry(countryCode) {
    const country = this.getCountryByCode(countryCode);
    return country ? country.phoneCode : null;
  }

  /**
   * Get validation regex for a country's phone number
   * @param {string} countryCode - Country code
   * @returns {string|null} Validation regex or null if not found
   */
  getPhoneValidationRegex(countryCode) {
    const country = this.getCountryByCode(countryCode);
    return country ? country.validationRegex : null;
  }

  /**
   * Get format example for a country's phone number
   * @param {string} countryCode - Country code
   * @returns {string|null} Format example or null if not found
   */
  getPhoneFormatExample(countryCode) {
    const country = this.getCountryByCode(countryCode);
    return country ? country.formatExample : null;
  }

  /**
   * Search countries by name
   * @param {string} searchTerm - Search term
   * @returns {Array} Array of matching countries
   */
  searchCountries(searchTerm) {
    const term = searchTerm.toLowerCase();
    return this.countries.filter(country => 
      country.name.toLowerCase().includes(term) ||
      country.code.toLowerCase().includes(term)
    );
  }

  /**
   * Get popular countries (commonly used ones)
   * @returns {Array} Array of popular country codes
   */
  getPopularCountries() {
    return [
      'US', 'GB', 'CA', 'AU', 'DE', 'FR', 'IN', 'JP', 'CN', 'BR',
      'IT', 'ES', 'NL', 'SE', 'NO', 'DK', 'FI', 'CH', 'AT', 'BE'
    ];
  }

  /**
   * Get countries sorted with popular ones first
   * @returns {Array} Array of countries with popular ones first
   */
  getCountriesSorted() {
    const popular = this.getPopularCountries();
    const popularCountries = [];
    const otherCountries = [];

    this.countries.forEach(country => {
      if (popular.includes(country.code)) {
        popularCountries.push(country);
      } else {
        otherCountries.push(country);
      }
    });

    // Sort popular countries by the order in popular array
    popularCountries.sort((a, b) => {
      return popular.indexOf(a.code) - popular.indexOf(b.code);
    });

    // Sort other countries alphabetically
    otherCountries.sort((a, b) => a.name.localeCompare(b.name));

    return [...popularCountries, ...otherCountries];
  }

  /**
   * Check if service is initialized
   * @returns {boolean} Initialization status
   */
  isInitialized() {
    return this.data !== null;
  }

  /**
   * Reset the service
   */
  reset() {
    this.data = null;
    this.countries = [];
    this.states = {};
    this.phoneCodes = [];
  }
}

// Export singleton instance
export const countryStateService = new CountryStateService();
