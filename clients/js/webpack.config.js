const path = require("path");

module.exports = {
    entry: "./dist/index.js",
    experiments: {
        outputModule: true,
    },
    output: {
        path: path.resolve(__dirname),
        filename: "index.js",
        libraryTarget: "module",
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
