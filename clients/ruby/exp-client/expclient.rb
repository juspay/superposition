require 'ffi'
require 'os'
require 'thread'
require 'dotenv'
Dotenv.load

module ExpClientLib
  extend FFI::Library
  extension = if OS.windows?
                "dll"
              elsif OS.mac?
                "dylib"
              else
                "so"
              end
  libpath = File.expand_path(File.join(__dir__, "#{ENV["SUPERPOSITION_LIB_PATH"]}/libexperimentation_client.#{extension}"))
  ffi_lib libpath

  attach_function :expt_new_client, [:string, :int, :string], :int
  attach_function :expt_start_polling_update, [:string], :void
  attach_function :expt_get_client, [:string], :string
  attach_function :expt_get_applicable_variant, [:string, :string, :int], :string
  attach_function :expt_get_satisfied_experiments, [:string, :string, :string], :string
  attach_function :expt_get_filtered_satisfied_experiments, [:string, :string, :string], :string
  attach_function :expt_get_running_experiments, [:string], :string
  attach_function :expt_free_string, [:string], :void
  attach_function :expt_last_error_message, [], :string
  attach_function :expt_last_error_length, [], :int
  attach_function :expt_free_client, [:string], :void

  class ExperimentationClient
    def initialize(tenant_name, polling_frequency, cac_host_name)
      @tenant = tenant_name
      @polling_frequency = polling_frequency
      @cac_host_name = cac_host_name
      @polling_thread = nil
    end

    def get_experimentation_last_error_message
      ExpClientLib.expt_last_error_message
    end

    def create_new_experimentation_client
      resp_code = ExpClientLib.expt_new_client(@tenant, @polling_frequency, @cac_host_name)
      if resp_code == 1
        error_message = get_experimentation_last_error_message
        puts "Some Error Occurred while creating new experimentation client: #{error_message}"
        raise "Client Creation Error"
      end
      resp_code
    end

    def get_experimentation_client
      ExpClientLib.expt_get_client(@tenant)
    end

    def get_running_experiments
      client_ptr = get_experimentation_client
      ExpClientLib.expt_get_running_experiments(client_ptr)
    end

    def free_string(str)
      ExpClientLib.expt_free_string(str)
    end

    def start_experimentation_polling_update
      @polling_thread = Thread.new do
        ExpClientLib.expt_start_polling_update(@tenant)
      end
    end

    def get_experimentation_last_error_length
      ExpClientLib.expt_last_error_length
    end

    def free_experimentation_client
      ExpClientLib.expt_free_client(get_experimentation_client)
    end

    def get_filtered_satisfied_experiments(context, filter_prefix)
      ExpClientLib.expt_get_filtered_satisfied_experiments(
        get_experimentation_client, context, filter_prefix
      )
    end

    def get_applicable_variant(context, toss)
      ExpClientLib.expt_get_applicable_variant(
        get_experimentation_client, context, toss
      )
    end

    def get_satisfied_experiments(context, filter_prefix)
      ExpClientLib.expt_get_satisfied_experiments(
        get_experimentation_client, context, filter_prefix
      )
    end
  end
end
