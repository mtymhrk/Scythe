HERE = File.expand_path(File.dirname(__FILE__)) + '/'

require 'rake'
require 'rake/clean'
require 'rake/testtask'
require HERE+'rakefile_helper'

include RakefileHelpers

# Load default configuration, for now
DEFAULT_CONFIG_FILE = 'gcc_64.yml'
configure_toolchain(DEFAULT_CONFIG_FILE)

task :unit do
  # run_test_cases_per_test
  run_test_cases_all_in_one_runner
end

task :clean_all => [:clean] do
  clean_test_case
  clean_unity
end

desc "Generate test summary"
task :summary do
  report_summary
end


desc "Build and test Unity"
task :all => [:clean_all, :unit, :summary]
task :default => [:clobber, :all]
task :ci => [:default]
task :cruise => [:default]

desc "Load configuration"
task :config, :config_file do |t, args|
  configure_toolchain(args[:config_file])
end
