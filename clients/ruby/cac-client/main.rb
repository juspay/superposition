require_relative 'cacclient'

# Example usage
begin
    puts "Starting the client"
    client = CACClient.new("dev", 60000, "http://localhost:8080")

    client.start_polling_update

    puts "Last Modified: #{client.get_last_modified}"

    config = client.get_config('{"country": "India"}', 'country')
    puts "Config: #{config}"

    resolved_config = client.get_resolved_config('{"country": "India"}',
    "country_image_url,hyperpay_version", MergeStrategy::REPLACE)
    puts "Resolved Config: #{resolved_config}"

    default_config = client.get_default_config("your_filter_keys")
    puts "Default Config: #{default_config}"
rescue StandardError => e
    puts "Error: #{e.message}"
ensure
    puts "Done with execution"
end
