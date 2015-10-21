require 'erb'

INDENT_STRING = '  '
NR_ELEMENTS = 16
COMMAND_OUTPUT_FILE = './marshal.out'

def exec_compile(cmd, file, output)
  `#{cmd} -ZZZ compile-file #{file} #{output}`
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

TARGETS = ['scythe/internal/syntax.scm',
           'scythe/internal/identifier.scm',
           'scythe/internal/cmpl-env.scm',
           'scythe/internal/compile.scm',
           'scythe/internal/dynamic-env.scm',
           'scythe/internal/macro.scm',
           'scythe/internal/record.scm',
           'scythe/internal/multiple-val.scm']

if ARGV.length < 4
  $stderr.puts 'too few arguments'
  exit 1
end

command_path, source_base, template_file, output_file = ARGV[0..4]
codes = TARGETS.each_with_object({}) do |source, tbl|
  key = source_name_to_symbol(source)
  path = File.join(source_base, source)
  tbl[key] = format_data(compiled_data(command_path, path))
end
template = ERB.new(File.read(template_file))
File.write(output_file, template.result(binding))
