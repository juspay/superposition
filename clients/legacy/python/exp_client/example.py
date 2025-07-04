from exp_client import ExperimentationClient
import http.server
import socketserver

try:
    tenant_name = "dev"
    polling_frequency = 1
    cac_host_name = "http://localhost:8080"
    exp_client = ExperimentationClient(tenant_name, polling_frequency, cac_host_name)
    exp_client.start_experimentation_polling_update()

    PORT = 8003

    Handler = http.server.SimpleHTTPRequestHandler

    class MyHandler(http.server.BaseHTTPRequestHandler):
        def do_GET(self):
            # Respond with a 200 OK status
            if self.path == '/testexp':
                expClientResp = exp_client.get_satisfied_experiments({})
                self.send_response(200)
                self.send_header("Content-type", "text/html")
                self.end_headers()
                self.wfile.write(str(expClientResp).encode())

    with socketserver.TCPServer(("", PORT), MyHandler) as httpd:
        print(f"Serving at port http://localhost:{PORT}")
        httpd.serve_forever()

except Exception as e:
    print("Error:", e)