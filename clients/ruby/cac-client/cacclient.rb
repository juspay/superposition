require 'ffi'
require 'os'

module MergeStrategy
    REPLACE = "REPLACE"
    MERGE = "MERGE"
end

module CACLib
    extend FFI::Library

    extension = "dylib"
    if OS.windows?
        extension = "dll"
    elsif OS.mac?
        extension = "dylib"
    else
        extension = "so"
    end
    libpath = File.expand_path(File.join(__dir__, "/../../../target/debug/libcac_client.#{extension}"))
    ffi_lib libpath

    class ArcClient < FFI::Struct
        layout :dummy, :int
    end

    # Attach C functions
    attach_function :cac_last_error_length, [], :int
    attach_function :cac_last_error_message, [], :string
    attach_function :cac_free_string, [:pointer], :void
    attach_function :cac_new_client, [:string, :ulong, :string], :int
    attach_function :cac_start_polling_update, [:string], :void
    attach_function :cac_free_client, [ArcClient.by_ref], :void
    attach_function :cac_get_client, [:string], ArcClient.by_ref
    attach_function :cac_get_last_modified, [ArcClient.by_ref], :string
    attach_function :cac_get_config, [ArcClient.by_ref, :string, :string], :string
    attach_function :cac_get_resolved_config, [ArcClient.by_ref, :string, :string, :string], :string
    attach_function :cac_get_default_config, [ArcClient.by_ref, :string], :string
end

class CACClient
  def initialize(tenant, update_frequency, hostname)
    result = CACLib.cac_new_client(tenant, update_frequency, hostname)
    raise "Failed to create client: #{last_error_message}" if result != 0
    @client = CACLib.cac_get_client(tenant)
    raise "Failed to get client: #{last_error_message}" if @client.null?
    @tenant = tenant
  end

  def start_polling_update
    CACLib.cac_start_polling_update(@tenant)
  end

  def get_last_modified
    result = CACLib.cac_get_last_modified(@client)
    raise "Failed to get last modified time: #{last_error_message}" if result.nil?
    result
  end

  def get_config(filter_query, filter_prefix)
    result = CACLib.cac_get_config(@client, filter_query, filter_prefix)
    puts "Filter query  : #{result}"
    raise "Failed to get config: #{last_error_message}" if result.nil?
    result
  ensure
    # CACLib.cac_free_string(result) if result // No need to free as its handled internally
  end

  def get_resolved_config(query, filter_keys, merge_strategy)
    result = CACLib.cac_get_resolved_config(@client, query, filter_keys, merge_strategy)
    raise "Failed to get resolved config: #{last_error_message}" if result.nil?
    result
  end

  def get_default_config(filter_keys)
    result = CACLib.cac_get_default_config(@client, filter_keys)
    raise "Failed to get default config: #{last_error_message}" if result.nil?
    result
  end

  def last_error_message
    CACLib.cac_last_error_message
  end

  def close
    CACLib.cac_free_client(@client) unless @client.null?
  end
end

# Example usage
begin
    client = CACClient.new("dev", 60000, "http://localhost:8080")
    puts "Starting the client"

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
    client.close if client
end
