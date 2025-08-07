import typescript from '@rollup/plugin-typescript';
import { nodeResolve } from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import json from '@rollup/plugin-json';
import copy from 'rollup-plugin-copy';

const external = [
    '@openfeature/server-sdk',
    'koffi',
    'fs', 
    'path', 
    'crypto', 
    'http', 
    'https', 
    'url', 
    'util', 
    'stream', 
    'events', 
    'buffer', 
    'os'
];

export default {
    input: 'index.ts',
    output: [
        {
            file: 'dist/index.js',
            format: 'cjs',
            sourcemap: true,
            exports: 'named',
            inlineDynamicImports: true
        },
        {
            file: 'dist/index.esm.js',
            format: 'es',
            sourcemap: true,
            inlineDynamicImports: true
        }
    ],
    external,
    plugins: [
        json(),
        copy({
            targets: [
                { src: '../bindings/native-lib/**/*', dest: 'dist/native-lib' }
            ],
            hook: 'buildStart'
        }),
        nodeResolve({
            preferBuiltins: true
        }),
        commonjs(),
        typescript({
            tsconfig: './tsconfig.json'
        })
    ]
};
