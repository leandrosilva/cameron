$LOAD_PATH.unshift(File.expand_path("."))

require "config/init"

run Rack::API
