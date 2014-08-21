require 'erb'

def exec_compile(cmd, file)
  `#{cmd} #{file}`.chomp
end

if ARGV.length < 4
  $stderr.puts "too few arguments"
  exit 1
end

command_path, source_file, template_file, output_file = ARGV[0..4]

File.open(output_file, "w") do |file|
  code = exec_compile(command_path, source_file)
  code = "\"" + code.gsub(/([\\"])/) { "\\#{$1}" } + "\""
  template = ERB.new(File.read(template_file))
  file.write(template.result(binding))
end
