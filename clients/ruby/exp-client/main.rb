require_relative 'expclient'

# Example usage
begin
    client = ExpClientLib::ExperimentationClient.new('dev', 10, 'http://localhost:8080')
    client.create_new_experimentation_client
    puts "Running experiments :: #{client.get_running_experiments}"
    client.start_experimentation_polling_update
    puts "After starting the polling"
    puts "Running get_filtered_satisfied_experiments :: #{client.get_filtered_satisfied_experiments("juspay", "key1")}"
rescue StandardError => e
    puts "Error: #{e.message}"
ensure
    puts "Done with the ExperimentationClient execution!"
end
