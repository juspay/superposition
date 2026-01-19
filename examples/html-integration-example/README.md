# Superposition Admin - HTML/JS Integration Example

This is a complete example project demonstrating how to integrate the Superposition admin interface into a vanilla HTML/JavaScript application.

## 🎯 What This Example Shows

This example showcases:

- ✅ **Basic Integration** - How to embed the admin interface in a standard web page
- ✅ **Theme Customization** - Switching between different themes dynamically
- ✅ **Dynamic Loading** - Loading the interface on-demand for better performance
- ✅ **Lifecycle Management** - Mounting and unmounting the interface
- ✅ **Multiple Pages** - Integration across different pages in an application
- ✅ **Responsive Design** - Mobile-friendly layout

## 📁 Project Structure

```
html-integration-example/
├── index.html           # Landing page with overview
├── embedded.html        # Full integration example
├── custom-theme.html    # Theme customization demo
├── dynamic.html         # Dynamic loading pattern
├── styles/
│   └── main.css        # Application styles
├── package.json         # Dependencies and scripts
└── README.md           # This file
```

## 🚀 Getting Started

### Prerequisites

- Node.js 14+ installed
- Superposition backend running (or accessible at a URL)

### Installation

1. **Navigate to this example directory:**

   ```bash
   cd examples/html-integration-example
   ```

2. **Install dependencies:**

   ```bash
   npm install
   ```

   This will install the Superposition admin package locally from the repository.

3. **Start the development server:**

   ```bash
   npm start
   ```

   This will start a local HTTP server at http://localhost:8000 and open it in your browser.

### Alternative: Manual Setup

If you prefer not to use npm:

```bash
# Using Python
python3 -m http.server 8000

# Using PHP
php -S localhost:8000

# Using any other static file server
```

Then open http://localhost:8000 in your browser.

## 🎨 Example Pages

### 1. Landing Page (`index.html`)

The main landing page provides an overview of the integration examples and quick start guide.

**Features:**
- Overview of integration capabilities
- Configuration options reference
- Quick start code snippets
- Links to detailed examples

### 2. Embedded Admin (`embedded.html`)

A complete example showing the admin interface embedded in a custom control panel.

**Features:**
- Mount/unmount controls
- Theme switching
- Backend host configuration
- Status indicators
- Persistent settings (localStorage)

**Key Code:**

```javascript
import SuperpositionAdmin from '@juspay/superposition-admin';

const admin = new SuperpositionAdmin({
    host: 'http://localhost:8080',
    theme: 'light',
    containerId: 'superposition-admin'
});

await admin.mount();
```

### 3. Custom Theme (`custom-theme.html`)

Demonstrates theme customization and dynamic theme switching.

**Features:**
- Visual theme previews
- Click to switch themes
- Live theme updates
- Theme selection before mounting

**Themes Available:**
- **Light** - Clean and bright
- **Dark** - Easy on the eyes
- **Cupcake** - Playful pink/purple
- **Dim** - Balanced dark theme

### 4. Dynamic Loading (`dynamic.html`)

Shows how to load the admin interface on-demand using a tabbed interface pattern.

**Features:**
- Multi-tab application layout
- Lazy loading of admin interface
- Only loads when Configuration tab is clicked
- Improved initial page load performance

**Performance Benefits:**
- Smaller initial bundle size
- Faster first contentful paint
- Better bandwidth utilization
- Loads only when needed

## 🔧 Configuration

### Backend Connection

By default, the examples try to connect to a Superposition backend at `http://localhost:8080`.

To change this:

1. **In the UI:** Enter your backend URL in the "Backend Host" input field
2. **In Code:** Modify the `host` option when creating the `SuperpositionAdmin` instance

```javascript
const admin = new SuperpositionAdmin({
    host: 'https://your-backend.example.com',  // Your backend URL
    // ... other options
});
```

### Running Superposition Backend

If you don't have a backend running, start one from the repository root:

```bash
# From the superposition repository root
make run
```

This will start the backend at http://localhost:8080.

## 📖 Code Walkthrough

### Basic Integration Pattern

```javascript
// 1. Import the library
import SuperpositionAdmin from '@juspay/superposition-admin';

// 2. Create an instance with configuration
const admin = new SuperpositionAdmin({
    host: 'http://localhost:8080',      // Backend API URL
    servicePrefix: '',                   // Optional URL prefix
    containerId: 'superposition-admin',  // Where to mount
    theme: 'light',                      // Initial theme
    pkgPath: './pkg'                     // Path to WASM files
});

// 3. Mount the interface
await admin.mount();

// 4. Later, unmount if needed
admin.unmount();
```

### Dynamic Theme Switching

```javascript
// Change theme after mounting
admin.setTheme('dark');

// Listen to theme changes in your app
themeSelector.addEventListener('change', (e) => {
    admin.setTheme(e.target.value);
});
```

### Lazy Loading Pattern

```javascript
let admin = null;

// Load only when needed
async function loadAdminOnDemand() {
    if (!admin) {
        admin = new SuperpositionAdmin({
            host: 'http://localhost:8080'
        });
        await admin.mount();
    }
}

// Trigger on user action
configButton.addEventListener('click', loadAdminOnDemand);
```

### Error Handling

```javascript
try {
    await admin.mount();
    console.log('Admin mounted successfully');
} catch (error) {
    console.error('Failed to mount admin:', error);
    // Show error to user
    showErrorMessage('Failed to load configuration interface');
}
```

## 🎯 Integration Patterns

### Pattern 1: Full Page Integration

Use when the admin interface is the main content of a page.

```html
<div id="superposition-admin"></div>
<script type="module">
    // Load immediately on page load
    import SuperpositionAdmin from '@juspay/superposition-admin';
    const admin = new SuperpositionAdmin({ host: 'http://localhost:8080' });
    admin.mount();
</script>
```

### Pattern 2: Tabbed Interface

Use when integrating into a multi-section application.

```javascript
// Load when tab is activated
tabs.forEach(tab => {
    tab.addEventListener('click', async () => {
        if (tab.id === 'config-tab' && !adminLoaded) {
            await admin.mount();
            adminLoaded = true;
        }
    });
});
```

### Pattern 3: Modal/Overlay

Use for on-demand configuration access.

```javascript
openConfigButton.addEventListener('click', async () => {
    showModal();
    if (!admin) {
        admin = new SuperpositionAdmin({
            host: 'http://localhost:8080',
            containerId: 'config-modal'
        });
        await admin.mount();
    }
});
```

## 🐛 Troubleshooting

### WASM Not Loading

**Problem:** Browser console shows errors loading `.wasm` files.

**Solutions:**
- Ensure your web server serves `.wasm` files with MIME type `application/wasm`
- Check CORS headers if loading from a different origin
- Verify the `pkgPath` option points to the correct location

**For local development:**
```bash
# The npm package includes WASM files in:
# node_modules/@juspay/superposition-admin/pkg/
```

### Backend Connection Failed

**Problem:** Admin interface loads but can't connect to backend.

**Solutions:**
- Verify backend is running and accessible
- Check the host URL is correct
- Ensure CORS is configured on the backend
- Check browser console for network errors

### Styling Issues

**Problem:** Admin interface doesn't look right.

**Solutions:**
- Check that CSS is being loaded from the package
- Verify no conflicting global styles
- Check browser console for 404s on `style.css`

### Module Import Errors

**Problem:** `Cannot use import statement outside a module`

**Solutions:**
- Use `<script type="module">` tag
- Ensure your build tool supports ES modules
- Check file paths in import statements

## 📚 Additional Resources

- [Superposition Documentation](https://github.com/juspay/superposition)
- [NPM Package README](../../crates/frontend/npm/README.md)
- [Development Guide](../../crates/frontend/npm/DEVELOPMENT.md)

## 🤝 Contributing

Found an issue or want to improve this example? Contributions are welcome!

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Submit a pull request

## 📄 License

This example is part of the Superposition project and is licensed under MIT OR Apache-2.0.

## 💬 Support

- GitHub Issues: https://github.com/juspay/superposition/issues
- Email: superposition@juspay.in

---

**Built with ❤️ using Superposition**
