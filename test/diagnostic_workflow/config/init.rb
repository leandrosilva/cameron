require "rubygems"

require "bundler/setup"
Bundler.require(:default)

require "yaml"

$ENVIRONMENT = ENV["RACK_ENV"] || "development"

require "api/process"
