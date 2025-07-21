import { execSync } from 'child_process';
import fs from 'fs';
import path from 'path';

function buildDependencies() {
    console.log('Building dependencies...');

    // Build SDK
    console.log('Building SDK...');
    execSync('cd ../sdk && npm run build', { stdio: 'inherit' });

    // Build bindings
    console.log('Building bindings...');
    execSync('cd ../bindings && npm run build', { stdio: 'inherit' });

    console.log('Dependencies built successfully!');
}

function copyDependencies() {
    console.log('Copying dependencies to node_modules...');

    // Ensure node_modules exists
    if (!fs.existsSync('node_modules')) {
        fs.mkdirSync('node_modules');
    }

    // Copy built dependencies
    execSync('cp -r ../sdk/dist-cjs node_modules/superposition-sdk', { stdio: 'inherit' });
    execSync('cp -r ../bindings/dist node_modules/superposition-bindings', { stdio: 'inherit' });

    console.log('Dependencies copied successfully!');
}

buildDependencies();
copyDependencies();
