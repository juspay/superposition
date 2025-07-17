import { themes as prismThemes } from "prism-react-renderer";
import type { Config } from "@docusaurus/types";
import type * as Preset from "@docusaurus/preset-classic";

// This runs in Node.js - Don't use client-side code here (browser APIs, JSX...)

const config: Config = {
    title: "Superposition",
    tagline:
        "Documentation of APIs, OpenFeature Providers and SDKs for Superposition",
    favicon: "img/favicon.ico",

    // Future flags, see https://docusaurus.io/docs/api/docusaurus-config#future
    future: {
        v4: true, // Improve compatibility with the upcoming Docusaurus v4
    },

    // Set the production url of your site here
    url: "https://juspay.io/",
    // Set the /<baseUrl>/ pathname under which your site is served
    // For GitHub pages deployment, it is often '/<projectName>/'
    baseUrl: "/superposition/",

    // GitHub pages deployment config.
    organizationName: "juspay",
    projectName: "superposition",

    onBrokenLinks: "warn",
    onBrokenMarkdownLinks: "warn",

    i18n: {
        defaultLocale: "en",
        locales: ["en"],
    },

    presets: [
        [
            "classic",
            {
                docs: {
                    routeBasePath: '/docs',
                    sidebarPath: "./sidebars.ts",
                    // Please change this to your repo.
                    // Remove this to remove the "edit this page" links.
                    // editUrl:
                    //   'https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/',
                },
                blog: false,
                theme: {
                    customCss: "./src/css/custom.css",
                },
            } satisfies Preset.Options,
        ],
    ],

    themeConfig: {
        // Replace with your project's social card
        image: "img/logo.jpg",
        navbar: {
            title: "Superposition Docs",
            logo: {
                alt: "Superposition",
                src: "https://juspay.io/images/superposition/logo.jpg",
            },
            items: [
                // {
                //     type: "docSidebar",
                //     sidebarId: "superpositionSidebar",
                //     position: "left",
                //     label: "Tutorial",
                // },
                // {to: '/blog', label: 'Blog', position: 'left'},
                {
                    href: "https://github.com/juspay/superposition",
                    label: "GitHub",
                    position: "right",
                },
            ],
        },
        footer: {
            style: "dark",
            links: [
                {
                    title: "Docs",
                    // items: [
                    //     {
                    //         label: "Tutorial",
                    //         to: "/",
                    //     },
                    // ],
                },
                {
                    title: "Community",
                    items: [
                        // {
                        //     label: "Discord",
                        //     href: "https://discordapp.com/invite/docusaurus",
                        // },
                        {
                            label: "X",
                            href: "https://x.com/superpositionJP",
                        },
                    ],
                },
                {
                    title: "More",
                    items: [
                        {
                            label: "Juspay",
                            to: "https://juspay.io",
                        },
                        {
                            label: "GitHub",
                            href: "https://github.com/juspay/superposition",
                        },
                    ],
                },
            ],
            copyright: `Copyright © ${new Date().getFullYear()} Juspay Technologies Private Limited. Built with Docusaurus.`,
        },
        prism: {
            theme: prismThemes.github,
            darkTheme: prismThemes.dracula,
        },
    } satisfies Preset.ThemeConfig,
};

export default config;
