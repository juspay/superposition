from prometheus_client import start_http_server, Counter, Gauge, Histogram
import random
import time
import os

# Read IDs from environment variable, default to 5 numeric IDs if not set
ids_str = os.environ.get("EXPORTER_IDS", "id_1,id_2,id_3,id_4,id_5")
ids = [id_.strip() for id_ in ids_str.split(",")]

# Metrics
request_count = Counter("custom_requests_total", "Number of requests", ["id"])
job_duration = Gauge("custom_job_duration_seconds", "Simulated job duration", ["id"])
job_latency = Histogram("custom_job_latency_seconds", "Simulated latency distribution", ["id"])

start_http_server(8090)
print("Serving metrics at :8090")

while True:
    for id_ in ids:
        # Simulate job duration with occasional spike
        duration = random.uniform(0.5, 2.0)
        if random.random() < 0.1:  # 10% chance of anomaly
            duration *= 5  # spike

        # Simulate latency
        latency = random.expovariate(1.0 / 0.8)

        # Update metrics
        request_count.labels(id=id_).inc()
        job_duration.labels(id=id_).set(duration)
        job_latency.labels(id=id_).observe(latency)

        print(f"[{id_}] requests+1, duration={duration:.2f}s, latency={latency:.2f}s")

    time.sleep(2)
