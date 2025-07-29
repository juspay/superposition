// attached to behaviours 0, 1 in behaviour_settings.png in this directory

function handler(event) {
  var request = event.request;
  var uri = request.uri;
  var headers = request.headers;
  var prefix = '/open-source';
  var newUri = uri;
  var newUrl;


  // Example: Redirect www.example.com to example.com
    if (uri.startsWith(prefix + '/superposition/docs')) {
        newUri = uri.substring(prefix.length); // Remove prefix
        newUrl = 'https://' + headers.host.value + newUri;
        var response = {
            statusCode: 301,
            statusDescription: 'Moved Permanently',
            headers: {
                'location': { value: newUrl }
            }
        };
        console.log('redirecting to ' + newUrl);

        return response;
    }

  // implies all urls are now /superposition/docs - just add the trailing slash if missing
  if (!newUri.endsWith('/')) {
      newUri += '/';
  }

  console.log('old request.uri is: + ' + uri + ' new request.uri is ' + newUri);

  request.uri = newUri;

  return request;
}
