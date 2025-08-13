# Superposition Hyperswitch Demo

A dynamic payment configuration system that demonstrates the power of [Superposition](https://github.com/juspay/superposition) for context-aware configuration management integrated with Hyperswitch payment processing.

## 🚀 Overview

This application showcases how Superposition can be used to dynamically generate payment forms based on various contextual dimensions such as payment connector, method, country, and other parameters. The system automatically adapts the user interface and required fields based on the selected configuration context.

## ✨ Key Features

- **Dynamic Form Generation**: Forms adapt automatically based on payment context
- **Context-Aware Configuration**: Uses Superposition for intelligent config resolution
- **Real-time Filtering**: Payment methods and types filter based on connector selection
- **Country/State Integration**: Automatic state population and phone code detection
- **Cryptocurrency Support**: Network mapping for various cryptocurrencies
- **Responsive Design**: Modern UI built with Tailwind CSS
- **Modular Architecture**: Clean separation of concerns with service-oriented design

## 🛠️ Technology Stack

- **Frontend**: Vanilla JavaScript (ES6+), HTML5, CSS3
- **Styling**: Tailwind CSS
- **Build Tool**: Vite
- **Configuration Engine**: Superposition
- **Payment Integration**: Hyperswitch

## 📋 Prerequisites

- Node.js (v16 or higher)
- npm or yarn package manager

## 🚀 Getting Started

### Installation

1. **Clone the repository**

   ```bash
   git clone git@github.com:sh-iv-am/superposition-hyperswitch-demo.git
   cd superposition-hyperswitch-demo
   ```

2. **Install dependencies**

   ```bash
   npm install
   ```

3. **Start the development server**

   ```bash
   npm run dev
   ```

4. **Open your browser**
   Navigate to `http://localhost:5173` (or the port shown in your terminal)

### Build for Production

```bash
npm run build
```

### Preview Production Build

```bash
npm run preview
```

## 🎯 Usage

### Basic Usage

1. **Select Configuration Context**: Use the sidebar to choose your payment processing context:

   - **Connector**: Payment processor (Stripe, Adyen, PayPal, etc.)
   - **Payment Method**: Type of payment (Card, Bank Debit, Wallet, Crypto)
   - **Payment Method Type**: Specific variant (Credit, ACH, PayPal, etc.)
   - **Country**: Market location
   - **Mandate Type**: Authorization requirements
   - **Billing/Shipping Details**: Collection preferences

2. **Dynamic Form Generation**: The main form area updates automatically based on your selections, showing only relevant fields for the chosen context.

3. **Sample Configurations**: Use the quick-start buttons to load pre-configured scenarios:
   - Credit Card payments
   - Bank transfers
   - Digital wallets
   - Cryptocurrency payments

### Configuration Context

The system uses multiple dimensions to determine the appropriate form fields:

```javascript
{
  connector: "stripe",
  payment_method: "card",
  payment_method_type: "credit",
  country: "US",
  mandate_type: "non_mandate",
  collect_billing_details_from_wallet_connector: "false",
  collect_shipping_details_from_wallet_connector: "false"
}
```

## 🏗️ Architecture

### Project Structure

```
├── src/
│   ├── components/          # UI components
│   │   ├── fieldRenderer.js    # Field rendering logic
│   │   ├── formGenerator.js    # Form generation engine
│   │   └── layoutRenderers.js  # Layout components
│   ├── config/              # Configuration files
│   │   ├── constants.js         # Static configuration data
│   │   └── mappings.js          # Data mappings and relationships
│   ├── services/            # Business logic services
│   │   ├── configurationService.js  # Superposition integration
│   │   ├── countryStateService.js   # Geographic data handling
│   │   └── filterService.js         # Dynamic filtering logic
│   ├── utils/               # Utility functions
│   │   ├── domUtils.js             # DOM manipulation helpers
│   │   └── fieldUtils.js           # Field processing utilities
│   └── app.js               # Main application controller
├── config.json             # Superposition configuration
├── countrystate.json       # Geographic data
├── index.html              # Main HTML file
├── superposition.js        # Superposition client library
└── vite.config.js          # Vite configuration
```

### Core Services

#### Configuration Service

Handles Superposition integration and configuration resolution:

- Loads and parses configuration contexts
- Evaluates rules based on current context
- Provides resolved configuration to form generator

#### Country State Service

Manages geographic data and relationships:

- Country and state/province mappings
- Phone code detection
- Address format validation

#### Filter Service

Implements dynamic filtering logic:

- Payment method filtering based on connector
- Payment type filtering based on method
- Context-aware option management

## 🔧 Configuration

### Superposition Configuration

The `config.json` file contains the Superposition configuration that defines:

- Context dimensions and their possible values
- Rules for field visibility and requirements
- Default values and validation rules

### Country/State Data

The `countrystate.json` file provides:

- Country codes and names
- State/province data for each country
- Phone country codes
- Address format information

### Payment Mappings

The `src/config/mappings.js` file defines:

- Cryptocurrency network mappings
- Payment method relationships
- Connector-specific configurations

## 🎨 Customization

### Adding New Payment Methods

1. Update `src/config/constants.js` with new payment method options
2. Add corresponding rules in `config.json`
3. Implement any specific field rendering logic in `src/components/fieldRenderer.js`

### Adding New Connectors

1. Add connector to `PAYMENT_CONFIG.connectors` in `src/config/constants.js`
2. Define supported payment methods in the filter service
3. Update Superposition rules in `config.json`

### Styling Customization

The application uses Tailwind CSS for styling. Key customization points:

- Modify CSS custom properties in `index.html` for theme colors
- Update Tailwind classes in component files
- Add custom styles in the `<style>` section of `index.html`

## 🧪 Development

### Code Organization

- **Components**: Reusable UI components with specific responsibilities
- **Services**: Business logic and external integrations
- **Utils**: Pure functions for common operations
- **Config**: Static data and configuration files

### Best Practices

- Use ES6+ features and modules
- Maintain separation of concerns
- Follow functional programming principles where possible
- Keep components focused and testable

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- [Superposition](https://github.com/juspay/superposition) - Context-aware configuration management
- [Hyperswitch](https://hyperswitch.io/) - Payment orchestration platform
- [Vite](https://vitejs.dev/) - Fast build tool
- [Tailwind CSS](https://tailwindcss.com/) - Utility-first CSS framework

## 📞 Support

For questions and support:

- Create an issue in this repository
- Check the [Superposition documentation](https://github.com/juspay/superposition)
- Review the [Hyperswitch documentation](https://docs.hyperswitch.io/)

---
