/** @type {import('tailwindcss').Config} */
module.exports = {
    content: [
        '*.html',
        './src/app.rs',
        './src/pages/**/*.rs',
        './src/components/**/*.rs',
        './src/hoc/**/*.rs',
    ],
    safelist: [
        "peer/concluded-checkbox",
        "peer/created-checkbox",
        "peer/inprogress-checkbox",
        "peer/discarded-checkbox",
        "peer-checked/discarded-checkbox:badge-neutral",
        "peer-checked/concluded-checkbox:badge-success",
        "peer-checked/created-checkbox:badge-info",
        "peer-checked/inprogress-checkbox:badge-warning",
        "peer-checked/paused-checkbox:badge-error",
        "peer-checked/discarded-checkbox:text-white",
        "peer-checked/concluded-checkbox:text-white",
        "peer-checked/created-checkbox:text-white",
        "peer-checked/inprogress-checkbox:text-white",
        "peer-checked/paused-checkbox:text-white",
    ],
    theme: {
        extend: {
            keyframes: {
                slideInFromBottom: {
                    from: {
                        transform: "translateY(100%)",
                        opacity: 0,
                    },
                    to: {
                        transform: "translateY(0)",
                        opacity: 1,
                    },
                },
                slideOutFromBottom: {
                    from: {
                        transform: "translateY(0)",
                        opacity: 1,
                    },
                    to: {
                        transform: "translateX(100%)",
                        opacity: 0,
                    },
                },
            },
            animation: {
                "slide-in-bottom": "slideInFromBottom 0.2s ease-out",
                "slide-out-bottom": "slideOutFromBottom 0.2s ease-in",
            },
        },
    },
    plugins: [require("daisyui")],
    daisyui: {
        themes: ["light", "dark", "cupcake", "dim"],
    },
};
