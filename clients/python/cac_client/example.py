from cac_client import CacClient
import http.server
import socketserver

try:
    tenant_name = "dev"
    polling_frequency = 1
    cac_host_name = "http://localhost:8080"
    cac_client = CacClient.create_new_client(tenant_name, polling_frequency, cac_host_name)

    cac_client.start_cac_polling_update()

    PORT = 8002

    Handler = http.server.SimpleHTTPRequestHandler

    class MyHandler(http.server.BaseHTTPRequestHandler):
        def do_GET(self):
            # Respond with a 200 OK status
            if self.path == '/testconfig':
                cacClientResp = cac_client.get_default_config()
                self.send_response(200)
                self.send_header("Content-type", "text/html")
                self.end_headers()
                self.wfile.write(str((cacClientResp)).encode())

    with socketserver.TCPServer(("", PORT), MyHandler) as httpd:
        print(f"Serving at port http://localhost:{PORT}")
        httpd.serve_forever()

except Exception as e:
    print("Error:", e)