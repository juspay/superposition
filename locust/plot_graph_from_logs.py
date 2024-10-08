import os
import json
import matplotlib.pyplot as plt
from collections import defaultdict

def read_json_objects_from_files(directory):
    path_latency_map = defaultdict(list)

    for filename in os.listdir(directory):
        if filename.endswith(".log"):
            file_path = os.path.join(directory, filename)
            with open(file_path, 'r') as f:
                for line in f:
                    try:
                        obj = json.loads(line.strip())

                        path = obj.get('span', {}).get('path')
                        latency = obj.get('fields', {}).get('latency')
                        if path and latency is not None:
                            latency_number = int(latency.split(" ")[0])
                            # print(f"obj in line {path} {latency_number}")
                            path_latency_map[path].append(latency_number)
                            # exit(1)

                    except json.JSONDecodeError:
                        print(f"Skipping invalid JSON line in {filename}")

    return path_latency_map

def plot_latency(path_latency_map):
    plt.figure(figsize=(10, 6))

    db_latency = path_latency_map.get("/superposition/config", [])
    redis_latency = path_latency_map.get("/superposition/config/fast", [])

    if db_latency:
        plt.plot(db_latency, label="DB", color='blue')
    if redis_latency:
        plt.plot(redis_latency, label="Redis", color='red')

    plt.xlabel('Request Count')
    plt.ylabel('Latency (ms)')
    plt.title('Latency comparison between DB and Redis')
    plt.legend(loc='upper right')
    upper_limit = max(max(db_latency, default=0), max(redis_latency, default=0))
    # print(f"upper_limit {upper_limit}")
    # plt.ylim(0, upper_limit * 1.1)
    # plt.xlim(0, upper_limit * 1.1)
    plt.grid(False)
    output_file = "server_latency.png"
    plt.savefig(output_file, format='png')
    print(f"Plot saved to {output_file}")
    plt.close()

directory = '<dir>'
path_latency_map = read_json_objects_from_files(directory)
# print(f"path latency map {path_latency_map}")
plot_latency(path_latency_map)
