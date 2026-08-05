// attached to CloudFront behaviours for path prefixes:
//   /docs/*, /blog/*, /docs_static/*, /sitemap.xml, /opensearch.xml
//
// The Docusaurus site is built with baseUrl "/" and served from GitHub Pages at
// juspay.github.io/superposition/. This function prepends "/superposition" to
// the origin request URI so that the browser-facing URLs have no
// "/superposition" prefix while the origin receives the path it expects.
//
// The noSlashRedirectStubsPlugin in docusaurus.config.ts emits "foo.html"
// redirect stubs so GitHub Pages never issues its own 301 that would leak the
// "juspay.github.io" origin to the browser. No trailing-slash manipulation is
// needed here.

function handler(event) {
  var request = event.request;
  var uri = request.uri;

  // Prepend the GitHub Pages project path so the origin receives
  // /superposition/docs/... instead of /docs/...
  request.uri = '/superposition' + uri;

  return request;
}
