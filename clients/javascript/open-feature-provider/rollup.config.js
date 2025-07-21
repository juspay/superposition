import typescript from '@rollup/plugin-typescript';
import { nodeResolve } from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import json from '@rollup/plugin-json';
import copy from 'rollup-plugin-copy';

const external = [
    '@openfeature/server-sdk',
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
            exports: 'auto'
        },
        {
            file: 'dist/index.esm.js',
            format: 'es',
            sourcemap: true
        }
    ],
    external,
    plugins: [
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
        json(),
        typescript({
            tsconfig: './tsconfig.json'
        })
    ]
};
