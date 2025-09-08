import { themes as prismThemes } from "prism-react-renderer";
import type { Config } from "@docusaurus/types";
import type * as Preset from "@docusaurus/preset-classic";
import type * as OpenApiPlugin from "docusaurus-plugin-openapi-docs";

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
    baseUrl: "/superposition/docs",

    // GitHub pages deployment config.
    organizationName: "juspay",
    projectName: "superposition",

    onBrokenLinks: "warn",
    onBrokenMarkdownLinks: "warn",

    i18n: {
        defaultLocale: "en",
        locales: ["en"],
    },

    plugins: [
        [
            'docusaurus-plugin-openapi-docs',
            {
                id: "superposition-api",
                docsPluginId: "classic",
                config: {
                    superposition: {
                        specPath: "../smithy/output/source/openapi/Superposition.openapi.json",
                        outputDir: "docs/api",
                        downloadUrl: "../smithy/output/source/openapi/Superposition.openapi.json",
                        sidebarOptions: {
                            groupPathsBy: "tag",
                            categoryLinkSource: "tag"
                        },
                    } satisfies OpenApiPlugin.Options,
                },
            },
        ],
    ],

    presets: [
        [
            "classic",
            {
                docs: {
                    routeBasePath: '',
                    sidebarPath: "./sidebars.ts",
                    docItemComponent: "@theme/ApiItem", // Derived from docusaurus-theme-openapi
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

    themes: ["docusaurus-theme-openapi-docs"], // export theme components

    themeConfig: {
        image: "img/logo.jpg",
        algolia: {
            appId: 'ZK6EG087JC',
            // Public API key: it is safe to commit it
            apiKey: '2d61980440a8ce3d5832392666e51c65',
            indexName: 'superposition-docusaurus-1',
        },
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
            additionalLanguages: ["java", "gradle", "toml", "bash"],
        },
    } satisfies Preset.ThemeConfig,
};

export default config;
