import json
import os
import re
import matplotlib.pyplot as plt
from datetime import datetime
import seaborn as sns
import matplotlib.dates as mdates

def extract_metrics(message):
    match = re.search(r'(Network|Latency) metrics .* max:\s*(\d+),\s*min:\s*(\d+),\s*avg:\s*(\d+)', message)
    if match:
        metric_type = match.group(1)
        max_val = int(match.group(2))
        min_val = int(match.group(3))
        avg_val = int(match.group(4))
        return metric_type, max_val, min_val, avg_val
    return None, None, None, None

def extract_data_from_files(directory):
    timestamps = []
    network_max_metrics = []
    network_min_metrics = []
    network_avg_metrics = []

    latency_max_metrics = []
    latency_min_metrics = []
    latency_avg_metrics = []

    for filename in os.listdir(directory):
        filepath = os.path.join(directory, filename)

        with open(filepath, 'r') as f:
            for line in f:
                try:
                    data = json.loads(line)
                    timestamp = data['timestamp']
                    message = data['fields']['message']

                    metric_type, max_val, min_val, avg_val = extract_metrics(message)

                    if metric_type is not None:
                        timestamps.append(datetime.strptime(timestamp, "%Y-%m-%dT%H:%M:%S.%fZ"))

                        if metric_type == "Network":
                            network_max_metrics.append(max_val)
                            network_min_metrics.append(min_val)
                            network_avg_metrics.append(avg_val)
                        elif metric_type == "Latency":
                            latency_max_metrics.append(max_val)
                            latency_min_metrics.append(min_val)
                            latency_avg_metrics.append(avg_val)
                except json.JSONDecodeError:
                    print(f"Error decoding JSON in file {filename}")
                except KeyError:
                    print(f"KeyError: Missing expected keys in file {filename}")

    return (timestamps, network_max_metrics, network_min_metrics, network_avg_metrics,
            latency_max_metrics, latency_min_metrics, latency_avg_metrics)

def plot_metrics(timestamps, network_max, network_min, network_avg, latency_max, latency_min, latency_avg):
    sns.set_theme(style="whitegrid", palette="pastel")  # Set seaborn style and palette

    fig, ax1 = plt.subplots(figsize=(12, 8))

    # Plot Network metrics on the first axis (left y-axis)
    sns.lineplot(x=timestamps, y=network_max, ax=ax1, label='Network Max Latency (ms)', color='red', marker='o')
    sns.lineplot(x=timestamps, y=network_min, ax=ax1, label='Network Min Latency (ms)', color='blue', marker='x', linestyle='--')
    sns.lineplot(x=timestamps, y=network_avg, ax=ax1, label='Network Avg Latency (ms)', color='green', marker='s', linestyle=':')

    ax1.set_xlabel('Timestamp', fontsize=12)
    ax1.set_ylabel('Network Latency (ms)', fontsize=12, color='blue')
    ax1.tick_params(axis='y', labelcolor='blue')
    ax1.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d %H:%M'))
    ax1.xaxis.set_major_locator(mdates.AutoDateLocator())
    plt.xticks(rotation=45)

    # Create a second y-axis to plot Latency metrics
    ax2 = ax1.twinx()
    sns.lineplot(x=timestamps, y=latency_max, ax=ax2, label='Latency Max (ms)', color='purple', marker='o', linestyle='-')
    sns.lineplot(x=timestamps, y=latency_min, ax=ax2, label='Latency Min (ms)', color='cyan', marker='x', linestyle='--')
    sns.lineplot(x=timestamps, y=latency_avg, ax=ax2, label='Latency Avg (ms)', color='yellow', marker='s', linestyle=':')

    ax2.set_ylabel('Latency (ms)', fontsize=12, color='green')
    ax2.tick_params(axis='y', labelcolor='green')

    plt.title('Network and Latency Metrics Over Time', fontsize=14)
    ax1.grid(True)

    lines_1, labels_1 = ax1.get_legend_handles_labels()
    lines_2, labels_2 = ax2.get_legend_handles_labels()
    ax1.legend(lines_1 + lines_2, labels_1 + labels_2, loc='upper left')

    fig.tight_layout()
    output_file = "redis_network_op_latency.png"
    plt.savefig(output_file, format='png')
    print(f"Plot saved to {output_file}")

def plot_histograms(network_max, network_min, network_avg, latency_max, latency_min, latency_avg):
    sns.set_theme(style="whitegrid", palette="pastel")  # Set Seaborn style and palette

    # Create a figure with multiple subplots for each metric type
    fig, axs = plt.subplots(2, 3, figsize=(18, 10))

    # Plot histograms for Network metrics
    sns.histplot(network_max, kde=True, color="blue", ax=axs[0, 0])
    axs[0, 0].set_title('Network Max Latency Distribution')

    sns.histplot(network_min, kde=True, color="blue", ax=axs[0, 1])
    axs[0, 1].set_title('Network Min Latency Distribution')

    sns.histplot(network_avg, kde=True, color="blue", ax=axs[0, 2])
    axs[0, 2].set_title('Network Avg Latency Distribution')

    # Plot histograms for Latency metrics
    sns.histplot(latency_max, kde=True, color="green", ax=axs[1, 0])
    axs[1, 0].set_title('Latency Max Distribution')

    sns.histplot(latency_min, kde=True, color="green", ax=axs[1, 1])
    axs[1, 1].set_title('Latency Min Distribution')

    sns.histplot(latency_avg, kde=True, color="green", ax=axs[1, 2])
    axs[1, 2].set_title('Latency Avg Distribution')

    # Set general plot properties
    for ax in axs.flat:
        ax.set_xlabel('Milliseconds')
        ax.set_ylabel('Frequency')

    plt.tight_layout()
    output_file = "redis_network_op_latency_hist.png"
    plt.savefig(output_file, format='png')
    print(f"Plot saved to {output_file}")

def plot_boxplots(network_max, network_min, network_avg, latency_max, latency_min, latency_avg):
    sns.set_theme(style="whitegrid", palette="pastel")  # Set Seaborn style and palette

    # Prepare the data for boxplots (each list of metrics will be part of a category)
    metrics_data = [
        network_max, network_min, network_avg,
        latency_max, latency_min, latency_avg
    ]
    metrics_labels = ['Network Max', 'Network Min', 'Network Avg', 'Latency Max', 'Latency Min', 'Latency Avg']

    # Create a figure with one subplot for the boxplot
    plt.figure(figsize=(12, 8))

    # Plot the boxplot with spines
    sns.boxplot(data=metrics_data)

    # Add titles and labels
    plt.title('Distribution of Network and Latency Metrics', fontsize=14)
    plt.xticks(range(6), metrics_labels, fontsize=12)
    plt.ylabel('Latency (ms)', fontsize=12)

    # Customize spines to make them more visible
    sns.despine(offset=10, trim=True)
    output_file = "redis_network_op_latency_box.png"
    plt.savefig(output_file, format='png')
    print(f"Plot saved to {output_file}")

def main():
    directory = '<dir>'
    (timestamps, network_max, network_min, network_avg, latency_max, latency_min, latency_avg) = extract_data_from_files(directory)
    # print(f"timestamps: {timestamps}")
    # print(f"network_max: {network_max}")
    # print(f"network_min: {network_min}")
    # print(f"network_avg: {network_avg}")
    # print(f"latency_max: {latency_max}")
    # print(f"latency_min: {latency_min}")
    # print(f"latency_avg: {latency_avg}")
    size = min([len(timestamps), len(network_max), len(network_min), len(network_avg), len(latency_max), len(latency_min), len(latency_avg)])
    if timestamps:
        plot_metrics(timestamps[:size], network_max[:size], network_min[:size], network_avg[:size], latency_max[:size], latency_min[:size], latency_avg[:size])
        plot_histograms(network_max[:size], network_min[:size], network_avg[:size], latency_max[:size], latency_min[:size], latency_avg[:size])
        plot_boxplots(network_max[:size], network_min[:size], network_avg[:size], latency_max[:size], latency_min[:size], latency_avg[:size])
    else:
        print("No valid data found.")

if __name__ == '__main__':
    main()
