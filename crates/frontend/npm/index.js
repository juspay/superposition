/**
 * Superposition Admin Frontend - NPM Package
 *
 * This package allows you to embed the Superposition admin interface
 * into any HTML/JavaScript application.
 */

class SuperpositionAdmin {
    constructor(options = {}) {
        this.options = {
            host: options.host || window.location.origin,
            servicePrefix: options.servicePrefix || '',
            containerId: options.containerId || 'superposition-admin',
            theme: options.theme || 'light', // 'light', 'dark', 'cupcake', 'dim'
            ...options
        };

        this.initialized = false;
        this.wasmModule = null;
    }

    /**
     * Initialize and mount the Superposition admin interface
     * @returns {Promise<void>}
     */
    async mount() {
        if (this.initialized) {
            console.warn('Superposition Admin is already initialized');
            return;
        }

        try {
            // Get or create container element
            let container = document.getElementById(this.options.containerId);
            if (!container) {
                container = document.createElement('div');
                container.id = this.options.containerId;
                document.body.appendChild(container);
            }

            // Set theme
            document.documentElement.setAttribute('data-theme', this.options.theme);

            // Set environment variables for the WASM module
            window.__APP_ENVS = {
                host: this.options.host,
                service_prefix: this.options.servicePrefix
            };

            // Load required dependencies
            await this._loadDependencies();

            // Load and initialize WASM module
            await this._initWasm();

            this.initialized = true;
            console.log('Superposition Admin mounted successfully');
        } catch (error) {
            console.error('Failed to mount Superposition Admin:', error);
            throw error;
        }
    }

    /**
     * Unmount the admin interface
     */
    unmount() {
        const container = document.getElementById(this.options.containerId);
        if (container) {
            container.innerHTML = '';
        }
        this.initialized = false;
        console.log('Superposition Admin unmounted');
    }

    /**
     * Change the theme dynamically
     * @param {string} theme - Theme name ('light', 'dark', 'cupcake', 'dim')
     */
    setTheme(theme) {
        this.options.theme = theme;
        document.documentElement.setAttribute('data-theme', theme);
    }

    /**
     * Load external dependencies required by the admin interface
     * @private
     */
    async _loadDependencies() {
        const dependencies = [
            {
                tag: 'link',
                attrs: {
                    href: 'https://cdn.jsdelivr.net/npm/remixicon/fonts/remixicon.css',
                    rel: 'stylesheet'
                }
            },
            {
                tag: 'script',
                attrs: {
                    src: 'https://cdn.jsdelivr.net/npm/@andypf/json-viewer@2.1.5/dist/iife/index.min.js'
                }
            },
            {
                tag: 'script',
                attrs: {
                    src: 'https://cdn.jsdelivr.net/npm/sortablejs@1.15.6/Sortable.min.js'
                }
            }
        ];

        const loadPromises = dependencies.map(dep => {
            return new Promise((resolve, reject) => {
                const existing = dep.tag === 'link'
                    ? document.querySelector(`link[href="${dep.attrs.href}"]`)
                    : document.querySelector(`script[src="${dep.attrs.src}"]`);

                if (existing) {
                    resolve();
                    return;
                }

                const element = document.createElement(dep.tag);
                Object.entries(dep.attrs).forEach(([key, value]) => {
                    element[key] = value;
                });

                element.onload = resolve;
                element.onerror = reject;

                document.head.appendChild(element);
            });
        });

        await Promise.all(loadPromises);
    }

    /**
     * Initialize the WASM module
     * @private
     */
    async _initWasm() {
        const pkgPath = this.options.pkgPath || './pkg';

        // Dynamically import the WASM module
        const wasmModule = await import(`${pkgPath}/frontend.js`);

        // Initialize WASM with the .wasm file
        await wasmModule.default(`${pkgPath}/frontend_bg.wasm`);

        // Call the hydrate function to mount the app
        wasmModule.hydrate();

        this.wasmModule = wasmModule;
    }
}

// Export for different module systems
if (typeof module !== 'undefined' && module.exports) {
    module.exports = SuperpositionAdmin;
}

if (typeof window !== 'undefined') {
    window.SuperpositionAdmin = SuperpositionAdmin;
}

export default SuperpositionAdmin;
