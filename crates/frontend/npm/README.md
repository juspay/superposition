# @juspay/superposition-admin

Embeddable admin interface for [Superposition](https://github.com/juspay/superposition) configuration management.

This package allows you to seamlessly integrate the Superposition admin UI into any HTML/JavaScript application, enabling configuration management, feature flags, experimentation, and more.

## Features

- 🚀 **Easy Integration** - Embed in any vanilla HTML/JS project
- 🎨 **Multiple Themes** - Light, dark, cupcake, and dim themes
- 🔧 **Configurable** - Customize host, service prefix, and container
- 📦 **Self-contained** - All dependencies bundled
- ⚡ **WebAssembly** - High-performance Rust/WASM implementation
- 🎯 **TypeScript Support** - Full type definitions included

## Installation

```bash
npm install @juspay/superposition-admin
```

## Quick Start

### Option 1: ES Modules (Recommended)

```html
<!DOCTYPE html>
<html>
<head>
    <title>My Admin Panel</title>
</head>
<body>
    <div id="superposition-admin"></div>

    <script type="module">
        import SuperpositionAdmin from '@juspay/superposition-admin';

        const admin = new SuperpositionAdmin({
            host: 'http://localhost:8080',  // Your Superposition backend URL
            servicePrefix: '',               // Optional: URL path prefix
            containerId: 'superposition-admin',
            theme: 'light'                   // 'light', 'dark', 'cupcake', or 'dim'
        });

        admin.mount().then(() => {
            console.log('Superposition Admin loaded successfully!');
        });
    </script>
</body>
</html>
```

### Option 2: CommonJS (Node.js/Webpack/Browserify)

```javascript
const SuperpositionAdmin = require('@juspay/superposition-admin');

const admin = new SuperpositionAdmin({
    host: 'http://localhost:8080',
    theme: 'dark'
});

admin.mount();
```

### Option 3: UMD (Browser Global)

```html
<!DOCTYPE html>
<html>
<head>
    <title>My Admin Panel</title>
    <script src="node_modules/@juspay/superposition-admin/index.js"></script>
</head>
<body>
    <div id="superposition-admin"></div>

    <script>
        const admin = new SuperpositionAdmin({
            host: 'http://localhost:8080'
        });

        admin.mount();
    </script>
</body>
</html>
```

## API Reference

### Constructor

```typescript
new SuperpositionAdmin(options?: SuperpositionAdminOptions)
```

#### Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `host` | `string` | `window.location.origin` | Backend API host URL |
| `servicePrefix` | `string` | `''` | Service prefix for routing (e.g., '/admin') |
| `containerId` | `string` | `'superposition-admin'` | Container element ID for mounting |
| `theme` | `'light' \| 'dark' \| 'cupcake' \| 'dim'` | `'light'` | UI theme |
| `pkgPath` | `string` | `'./pkg'` | Path to WASM artifacts directory |

### Methods

#### `mount(): Promise<void>`

Initialize and mount the admin interface.

```javascript
await admin.mount();
```

#### `unmount(): void`

Unmount the admin interface and clean up resources.

```javascript
admin.unmount();
```

#### `setTheme(theme: string): void`

Change the theme dynamically.

```javascript
admin.setTheme('dark');
```

## Advanced Usage

### Custom Container

```javascript
// Create your own container element
const container = document.createElement('div');
container.id = 'my-custom-admin';
document.querySelector('#app').appendChild(container);

const admin = new SuperpositionAdmin({
    host: 'http://localhost:8080',
    containerId: 'my-custom-admin'
});

admin.mount();
```

### Dynamic Theme Switching

```javascript
const admin = new SuperpositionAdmin({
    host: 'http://localhost:8080'
});

await admin.mount();

// Add theme switcher
document.getElementById('theme-toggle').addEventListener('click', () => {
    const themes = ['light', 'dark', 'cupcake', 'dim'];
    const currentIndex = themes.indexOf(admin.options.theme);
    const nextTheme = themes[(currentIndex + 1) % themes.length];
    admin.setTheme(nextTheme);
});
```

### Integration with React

```jsx
import { useEffect, useRef } from 'react';
import SuperpositionAdmin from '@juspay/superposition-admin';

function AdminPanel() {
    const adminRef = useRef(null);

    useEffect(() => {
        const admin = new SuperpositionAdmin({
            host: process.env.REACT_APP_SUPERPOSITION_HOST,
            containerId: 'superposition-admin',
            theme: 'light'
        });

        admin.mount();
        adminRef.current = admin;

        return () => {
            admin.unmount();
        };
    }, []);

    return <div id="superposition-admin" />;
}
```

### Integration with Vue

```vue
<template>
    <div id="superposition-admin"></div>
</template>

<script>
import SuperpositionAdmin from '@juspay/superposition-admin';

export default {
    name: 'AdminPanel',
    mounted() {
        this.admin = new SuperpositionAdmin({
            host: process.env.VUE_APP_SUPERPOSITION_HOST,
            theme: 'light'
        });
        this.admin.mount();
    },
    beforeUnmount() {
        if (this.admin) {
            this.admin.unmount();
        }
    }
}
</script>
```

### Integration with Angular

```typescript
import { Component, OnInit, OnDestroy } from '@angular/core';
import SuperpositionAdmin from '@juspay/superposition-admin';

@Component({
    selector: 'app-admin-panel',
    template: '<div id="superposition-admin"></div>'
})
export class AdminPanelComponent implements OnInit, OnDestroy {
    private admin: any;

    ngOnInit() {
        this.admin = new SuperpositionAdmin({
            host: environment.superpositionHost,
            theme: 'light'
        });
        this.admin.mount();
    }

    ngOnDestroy() {
        if (this.admin) {
            this.admin.unmount();
        }
    }
}
```

## Serving Static Assets

The package includes WASM and CSS files that need to be served. If you're using a bundler:

### Webpack

```javascript
// webpack.config.js
module.exports = {
    // ...
    module: {
        rules: [
            {
                test: /\.wasm$/,
                type: 'javascript/auto',
                loader: 'file-loader'
            }
        ]
    }
};
```

### Vite

```javascript
// vite.config.js
export default {
    assetsInclude: ['**/*.wasm']
};
```

### Manual Setup

If not using a bundler, ensure the `pkg` and `assets` directories are accessible:

```
your-app/
├── node_modules/
│   └── @juspay/superposition-admin/
│       ├── pkg/
│       │   ├── frontend.js
│       │   ├── frontend_bg.wasm
│       │   └── style.css
│       └── assets/
```

## Browser Compatibility

- Chrome/Edge: 89+
- Firefox: 89+
- Safari: 15+

WebAssembly support is required.

## Features Included

The admin interface provides comprehensive configuration management:

- **Organizations & Workspaces** - Multi-tenant management
- **Default Configuration** - Base configuration management
- **Dimensions** - Context dimensions for targeting
- **Context Overrides** - Conditional configuration overrides
- **Experiments** - A/B testing and experimentation
- **Functions** - Custom logic and transformations
- **Variables** - Dynamic configuration variables
- **Type Templates** - Schema management
- **Audit Log** - Change tracking and history
- **Webhooks** - Event notifications
- **Config Versions** - Version management and rollback

## Troubleshooting

### WASM Loading Issues

If you see errors loading WASM files:

1. Ensure your server is configured to serve `.wasm` files with the correct MIME type:
   ```
   application/wasm
   ```

2. Check CORS headers if loading from a different origin

3. Verify the `pkgPath` option points to the correct location

### Styling Issues

Make sure the CSS is being loaded. Check the browser console for 404 errors on `style.css`.

### TypeScript Errors

If using TypeScript, ensure the package types are recognized:

```typescript
import SuperpositionAdmin from '@juspay/superposition-admin';
// If types are not found, add to tsconfig.json:
// "include": ["node_modules/@juspay/superposition-admin/index.d.ts"]
```

## Development

To build the package from source:

```bash
# From the repository root
make npm-package
```

This will:
1. Build the Rust/WASM frontend
2. Compile Tailwind CSS
3. Copy assets
4. Prepare the npm package structure

## License

MIT OR Apache-2.0

## Links

- [Superposition Repository](https://github.com/juspay/superposition)
- [Documentation](https://juspay.io/open-source/superposition)
- [Report Issues](https://github.com/juspay/superposition/issues)

## Support

For questions and support:
- GitHub Issues: https://github.com/juspay/superposition/issues
- Email: superposition@juspay.in
