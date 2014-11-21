require 'erb'

INDENT_STRING = '  '
NR_ELEMENTS = 16
COMMAND_OUTPUT_FILE = './marshal.out'

def exec_compile(cmd, file)
  `#{cmd} #{file}`
end

if ARGV.length < 4
  $stderr.puts 'too few arguments'
  exit 1
end

command_path, source_file, template_file, output_file = ARGV[0..4]

File.open(output_file, 'w') do |file|
  exec_compile(command_path, source_file)
  data = File.open(COMMAND_OUTPUT_FILE, 'rb').read.unpack('C*')
  File.unlink(COMMAND_OUTPUT_FILE)
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
  template = ERB.new(File.read(template_file))
  file.write(template.result(binding))
end
