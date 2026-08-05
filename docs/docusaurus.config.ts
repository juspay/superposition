import { themes as prismThemes } from "prism-react-renderer";
import type { Config } from "@docusaurus/types";
import type * as Preset from "@docusaurus/preset-classic";
import type * as OpenApiPlugin from "docusaurus-plugin-openapi-docs";
import * as fs from "fs";
import * as path from "path";

// This runs in Node.js - Don't use client-side code here (browser APIs, JSX...)

// Relocate all webpack build output (JS, CSS, images) under a unique
// "docs_static/assets/" prefix so the CloudFront proxy can forward a clean,
// bounded set of path prefixes without colliding with the root app at "/".
function docsStaticAssetsPlugin() {
  return {
    name: "docs-static-assets",
    configureWebpack(config: any, isServer: boolean) {
      if (isServer || config.mode !== "production") return {};
      for (const plugin of config.plugins ?? []) {
        if (plugin?.constructor?.name?.includes("CssExtract") && plugin.options) {
          plugin.options.filename = "docs_static/assets/css/[name].[contenthash:8].css";
          plugin.options.chunkFilename = "docs_static/assets/css/[name].[contenthash:8].css";
        }
      }
      return {
        output: {
          filename: "docs_static/assets/js/[name].[contenthash:8].js",
          chunkFilename: "docs_static/assets/js/[name].[contenthash:8].js",
          assetModuleFilename: "docs_static/assets/[name].[contenthash:8][ext]",
        },
      };
    },
  };
}

// Emit a small "foo.html" stub at each no-slash path that redirects to the
// canonical "/foo/" with a root-relative URL. Because "foo.html" exists,
// GitHub Pages serves it directly and never issues its own redirect that
// would leak the "juspay.github.io" origin to the browser.
function noSlashRedirectStubsPlugin() {
  return {
    name: "no-slash-redirect-stubs",
    async postBuild({ outDir }: { outDir: string }) {
      const indexFiles: string[] = [];
      (function collect(dir: string) {
        for (const e of fs.readdirSync(dir, { withFileTypes: true })) {
          const full = path.join(dir, e.name);
          if (e.isDirectory()) collect(full);
          else if (e.name === "index.html") indexFiles.push(full);
        }
      })(outDir);
      for (const file of indexFiles) {
        const relDir = path.relative(outDir, path.dirname(file));
        if (relDir === "") continue;
        const urlPath = "/" + relDir.split(path.sep).join("/") + "/";
        const stub = path.join(outDir, relDir + ".html");
        if (fs.existsSync(stub)) continue;
        fs.writeFileSync(
          stub,
          `<!doctype html><html lang="en"><head><meta charset="utf-8">` +
            `<meta http-equiv="refresh" content="0; url=${urlPath}">` +
            `<title>Redirecting…</title></head>` +
            `<body>Redirecting to <a href="${urlPath}">${urlPath}</a>…</body></html>\n`,
        );
      }
    },
  };
}

const config: Config = {
    title: "Superposition",
    tagline:
        "Documentation of APIs, OpenFeature Providers and SDKs for Superposition",
    favicon: "docs_static/img/favicon.ico",

    // Future flags, see https://docusaurus.io/docs/api/docusaurus-config#future
    future: {
        v4: true, // Improve compatibility with the upcoming Docusaurus v4
    },

    // Set the production url of your site here
    url: "https://superposition.juspay.io",
    // Set the /<baseUrl>/ pathname under which your site is served
    // baseUrl is "/" because the site is served behind a CloudFront proxy that
    // forwards specific path prefixes (/docs, /blog, /docs_static, etc.) to the
    // GitHub Pages origin. We don't want "/superposition" in any URL.
    baseUrl: "/",
    trailingSlash: true,

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
        docsStaticAssetsPlugin,
        noSlashRedirectStubsPlugin,
        [
            'docusaurus-plugin-openapi-docs',
            {
                id: "superposition-api",
                docsPluginId: "classic",
                config: {
                    superposition: {
                        specPath: "docs/api/Superposition.openapi.json",
                        outputDir: "docs/api",
                        downloadUrl: "docs/api/Superposition.openapi.json",
                        sidebarOptions: {
                            groupPathsBy: "tag",
                            categoryLinkSource: "tag",
                            sidebarCollapsed: false,
                            customProps: {
                                // Add custom CSS classes for styling
                            }
                        },
                        // Removed template configuration that was causing the error
                        hideSendButton: false,
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
                    routeBasePath: '/docs',
                    sidebarPath: "./sidebars.ts",
                    docItemComponent: "@theme/ApiItem", // Derived from docusaurus-theme-openapi
                    // Please change this to your repo.
                    // Remove this to remove the "edit this page" links.
                    // editUrl:
                    //   'https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/',
                },
                blog: {
                    path: "blog",
                    routeBasePath: "blog",
                    blogTitle: "Superposition Blog",
                    blogDescription: "Updates, guides, and technical notes from the Superposition team",
                    showReadingTime: true,
                },
                theme: {
                    customCss: "./src/css/custom.css",
                },
            } satisfies Preset.Options,
        ],
    ],

    themes: ["docusaurus-theme-openapi-docs"], // export theme components

    themeConfig: {
        image: "docs_static/img/logo.jpg",
        algolia: {
            appId: 'ZK6EG087JC',
            // Public API key: it is safe to commit it
            apiKey: 'a9402301014892a68b227519cfab738a',
            indexName: 'superposition-docusaurus-1',
        },
        navbar: {
            title: "Superposition Docs",
            logo: {
                alt: "Superposition",
                src: "https://juspay.io/images/superposition/logo.jpg",
                href: "/docs",
            },
            items: [
                // {
                //     type: "docSidebar",
                //     sidebarId: "superpositionSidebar",
                //     position: "left",
                //     label: "Tutorial",
                // },
                { to: "/blog", label: "Blog", position: "left" },
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
