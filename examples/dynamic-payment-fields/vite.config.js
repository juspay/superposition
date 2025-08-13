import { defineConfig } from 'vite';
import { resolve } from 'path';
import { copyFileSync, existsSync } from 'fs';

export default defineConfig({
  // Build configuration
  build: {
    // Output directory
    outDir: 'dist',
    
    // Clean the output directory before building
    emptyOutDir: true,
    
    // Generate source maps for debugging
    sourcemap: true,
    
    // Minification settings
    minify: 'terser',
    terserOptions: {
      compress: {
        drop_console: true, // Remove console.log statements
        drop_debugger: true, // Remove debugger statements
        pure_funcs: ['console.log', 'console.warn'], // Remove specific functions
      },
      mangle: {
        toplevel: true, // Mangle top-level variable names
      },
      format: {
        comments: false, // Remove comments
      },
    },
    
    // Copy static files to output directory
    copyPublicDir: false, // We'll handle copying manually
    
    // Rollup options
    rollupOptions: {
      input: {
        main: resolve(__dirname, 'index.html'),
      },
      output: {
        // Chunk splitting for better caching
        manualChunks: {
          utils: [
            './src/utils/domUtils.js',
            './src/utils/fieldUtils.js'
          ],
          services: [
            './src/services/configurationService.js',
            './src/services/countryStateService.js',
            './src/services/filterService.js'
          ],
          components: [
            './src/components/fieldRenderer.js',
            './src/components/formGenerator.js',
            './src/components/layoutRenderers.js'
          ]
        },
        // Asset naming
        assetFileNames: 'assets/[name]-[hash][extname]',
        chunkFileNames: 'assets/[name]-[hash].js',
        entryFileNames: 'assets/[name]-[hash].js',
      },
    },
    
    // Asset handling
    assetsDir: 'assets',
    
    // Target modern browsers for better optimization
    target: 'es2020',
    
    // CSS code splitting
    cssCodeSplit: true,
  },
  
  // Plugin to copy static files
  plugins: [
    {
      name: 'copy-static-files',
      writeBundle() {
        // Copy static files to dist directory
        const filesToCopy = [
          'superposition.js',
          'config.json',
          'countrystate.json'
        ];
        
        filesToCopy.forEach(file => {
          if (existsSync(file)) {
            copyFileSync(file, `dist/${file}`);
            console.log(`✓ Copied ${file} to dist/`);
          } else {
            console.warn(`⚠ File ${file} not found, skipping...`);
          }
        });
      }
    }
  ],
  
  // Development server configuration
  server: {
    port: 3000,
    open: true,
    cors: true,
  },
  
  // Preview server configuration
  preview: {
    port: 4173,
    open: true,
  },
  
  // Base public path
  base: './',
  
  // Asset optimization
  assetsInclude: ['**/*.json'],
});
