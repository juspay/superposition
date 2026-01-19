/**
 * Superposition Admin Frontend - TypeScript Definitions
 */

export interface SuperpositionAdminOptions {
    /**
     * Backend API host URL
     * @default window.location.origin
     */
    host?: string;

    /**
     * Service prefix for routing (e.g., '/admin')
     * @default ''
     */
    servicePrefix?: string;

    /**
     * Container element ID where the admin UI will be mounted
     * @default 'superposition-admin'
     */
    containerId?: string;

    /**
     * Theme for the admin interface
     * @default 'light'
     */
    theme?: 'light' | 'dark' | 'cupcake' | 'dim';

    /**
     * Path to the pkg directory containing WASM artifacts
     * @default './pkg'
     */
    pkgPath?: string;
}

/**
 * Superposition Admin Interface
 *
 * Embeddable admin interface for Superposition configuration management
 */
export default class SuperpositionAdmin {
    /**
     * Configuration options
     */
    options: Required<SuperpositionAdminOptions>;

    /**
     * Whether the admin interface is initialized
     */
    initialized: boolean;

    /**
     * Reference to the loaded WASM module
     */
    wasmModule: any | null;

    /**
     * Create a new Superposition Admin instance
     * @param options - Configuration options
     */
    constructor(options?: SuperpositionAdminOptions);

    /**
     * Initialize and mount the Superposition admin interface
     * @returns Promise that resolves when mounting is complete
     */
    mount(): Promise<void>;

    /**
     * Unmount the admin interface and clean up resources
     */
    unmount(): void;

    /**
     * Change the theme dynamically
     * @param theme - Theme name
     */
    setTheme(theme: 'light' | 'dark' | 'cupcake' | 'dim'): void;
}
