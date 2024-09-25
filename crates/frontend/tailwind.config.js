/** @type {import('tailwindcss').Config} */
module.exports = {
    content: [
        '*.html',
        './src/app.rs',
        './src/pages/**/*.rs',
        './src/components/**/*.rs',
        './src/hoc/**/*.rs',
    ],
    theme: {
        extend: {
            keyframes: {
                slideInFromBottom: {
                    from: {
                        transform: 'translateY(100%)',
                        opacity: 0,
                    },
                    to: {
                        transform: 'translateY(0)',
                        opacity: 1,
                    },
                },
                slideOutFromBottom: {
                    from: {
                        transform: 'translateY(0)',
                        opacity: 1,
                    },
                    to: {
                        transform: 'translateX(100%)',
                        opacity: 0,
                    },
                },
            },
            animation: {
                'slide-in-bottom': 'slideInFromBottom 0.2s ease-out',
                'slide-out-bottom': 'slideOutToBottom 0.2s ease-in',
            },
            colors: {
                'surface': '#fafafa',
                'surface-2': '#f2ecee',
                'surface-3': '#f8f1f6',
            },
            height: {
                'main-content': 'calc(100vh - 16px - 12px - 12px - 16px - 36px - 32px - 32px)'
            }
        },
    },
    plugins: [require('daisyui')],
    daisyui: {
        themes: ['light', 'dark', 'cupcake', 'dim'],
    },
}
