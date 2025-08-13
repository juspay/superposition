/**
 * Configuration Service
 * Handles Superposition client initialization and configuration management
 */

export class ConfigurationService {
  constructor() {
    this.cacClient = null;
    this.configData = null;
  }

  /**
   * Initialize the Superposition client
   * @returns {Promise<boolean>} Success status
   */
  async initialize() {
    try {
      const response = await fetch('config.json');
      this.configData = await response.json();
      this.cacClient = new window["Context-Aware-Config"].CacReader(this.configData);
      console.log('SuperpositionClient initialized successfully');
      return true;
    } catch (error) {
      console.error('Error loading configuration:', error);
      return false;
    }
  }

  /**
   * Evaluate configuration based on context
   * @param {Object} context - Context object with connector, payment method, etc.
   * @returns {Object|null} Resolved configuration or null if error
   */
  evaluateConfiguration(context) {
    if (!this.cacClient) {
      console.error('Configuration service not initialized');
      return null;
    }

    try {
      console.log('Evaluating context:', JSON.stringify(context));
      const resolvedConfig = this.cacClient.evaluateConfig(context);
      console.log('Resolved config:', JSON.stringify(resolvedConfig, null, 2));
      return resolvedConfig;
    } catch (error) {
      console.error('Error evaluating configuration:', error);
      return null;
    }
  }

  /**
   * Check if service is initialized
   * @returns {boolean} Initialization status
   */
  isInitialized() {
    return this.cacClient !== null;
  }

  /**
   * Get configuration data
   * @returns {Object|null} Configuration data
   */
  getConfigData() {
    return this.configData;
  }

  /**
   * Reset the service
   */
  reset() {
    this.cacClient = null;
    this.configData = null;
  }
}

// Export singleton instance
export const configurationService = new ConfigurationService();
