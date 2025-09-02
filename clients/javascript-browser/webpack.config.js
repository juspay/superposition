const path = require("path");

module.exports = {
    entry: {
        index: "./dist/index.js",
        indexJsonlogic: "./dist/indexJsonlogic.js",
    },
    output: {
        path: path.resolve(__dirname),
        filename: "[name].js",
        library: {
            name: "[name]-Context-AwareConfig",
            type: "umd",
        },
        environment: {
            arrowFunction: false,
            bigIntLiteral: false,
            const: true,
            destructuring: false,
            dynamicImport: false,
            forOf: true,
            module: true,
        },
    },
    module: {
        rules: [
            {
                test: /\.js$/,
                exclude: /node_modules/,
                use: {
                    loader: "babel-loader",
                    options: {
                        presets: ["@babel/preset-env"],
                    },
                },
            },
        ],
    },
};
