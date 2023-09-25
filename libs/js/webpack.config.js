const path = require('path');

module.exports = {
  entry: './dist/index.js',
  output: {
    path: path.resolve(__dirname),
    filename: "index.js",
    library : {
        name : "Context-Aware-Config",
        type: "umd"
    },
    environment: {
      arrowFunction: false,
      bigIntLiteral: false,
      const: true,
      destructuring: false,
      dynamicImport: false,
      forOf: true,
      module: true
    }
  },
  module: {
    rules: [
      { test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: "babel-loader",
          options: {
            presets: ["@babel/preset-env"],
          }
        }
      },
    ]
  }
};

