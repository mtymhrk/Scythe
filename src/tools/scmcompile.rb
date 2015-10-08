require 'erb'

INDENT_STRING = '  '
NR_ELEMENTS = 16
COMMAND_OUTPUT_FILE = './marshal.out'

def exec_compile(cmd, file, output)
  `#{cmd} -o #{output} #{file}`
end

def compiled_data(cmd, file)
  exec_compile(cmd, file, COMMAND_OUTPUT_FILE)
  File.open(COMMAND_OUTPUT_FILE, 'rb').read.unpack('C*')
ensure
  File.unlink(COMMAND_OUTPUT_FILE) if File.exist?(COMMAND_OUTPUT_FILE)
end

def format_data(data)
  code = ''
  data.each_with_index do |x, i|
    code << INDENT_STRING if i % NR_ELEMENTS == 0
    code << sprintf('0x%02x', x)
    code << ',' if i < data.length - 1
    if (i % NR_ELEMENTS == NR_ELEMENTS - 1) || (i == data.length - 1)
      code << "\n"
    else
      code << ' '
    end
  end
  code
end

def source_name_to_symbol(name)
  name.gsub('/', '_').gsub('-', '').gsub('.scm', '').intern
end

if ARGV.length < 4
  $stderr.puts 'too few arguments'
  exit 1
end

targets = ['scythe/internal/compile.scm',
           'scythe/internal/dynamic-env.scm',
           'scythe/internal/macro.scm']

command_path, source_base, template_file, output_file = ARGV[0..4]

File.open(output_file, 'w') do |file|
  codes = targets.each_with_object({}) do |source, tbl|
    key = source_name_to_symbol(source)
    path = File.join(source_base, source)
    tbl[key] = format_data(compiled_data(command_path, path))
  end
  template = ERB.new(File.read(template_file))
  file.write(template.result(binding))
end
