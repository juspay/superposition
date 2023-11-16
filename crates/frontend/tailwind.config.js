/** @type {import('tailwindcss').Config} */
module.exports = {
    content: [
        "*.html",
        "./src/app.rs",
        "./src/pages/**/*.rs",
        "./src/components/**/*.rs",
        "./src/hoc/**/*.rs"
    ],
    theme: {
      extend: {},
    },
    plugins: [
        require("daisyui")
    ],
}