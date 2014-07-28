#!/usr/bin/ruby
# -*- coding:utf-8 -*-

require 'stringio'
require 'optparse'

require "#{File.expand_path(File.dirname(__FILE__))}/summarize_result"

def parse_options(argv)
  rslt = { :verbose => false, :group => nil, :name => nil, :repeat => nil }

  opt = OptionParser.new
  opt.on('-v')       {|v| rslt[:verbose] = true }
  opt.on('-g GROUP') {|v| rslt[:group]   = v }
  opt.on('-n NAME')  {|v| rslt[:name]    = v }
  # opt.on('-r NUM')   {|v| rslt[:repeat]  = v }  # repeat オプションは無視
  opt.parse!(ARGV)

  rslt
end

def command_line(cmd, opt)
  line = [ cmd ]

  line.push("-v") if opt[:verbose]
  unless opt[:group].nil?
    line.push("-g")
    line.push(opt[:group])
  end
  unless opt[:name].nil?
    line.push("-n")
    line.push(opt[:name])
  end
  ## 出力の解析が複数回実行に対応していないので repeat オプションは無視
  # unless opt[:repeat].nil?
  #   line.push("-r")
  #   line.push(opt[:repeat])
  # end

  line
end

def invoke_runner(runner, opt)
  prev_sync = $stdout.sync
  cmd = command_line(runner, opt)
  output = StringIO.new("", 'r+')

  $stdout.sync = true

  IO.popen([*cmd]) do |pipe|
    stat = :none
    n = 0
    while c = pipe.getc do
      output.print(c)
      case stat
      when :none
        stat = :newl if c == "\n"
      when :newl
        stat = if c == "\n"
                 :newl
               elsif c == "-"
                 n = 1
                 :deli
               else
                 :none
               end
      when :deli
        stat = if c == "\n"
                 :summ
               elsif c != "-"
                 $stdout.print("-" * n)
                 :none
               else
                 n += 1
                 :deli
               end
      when :summ
        break
      end

      $stdout.print(c) unless stat == :deli
    end

    pipe.each_line {|line| output.print(line) }
  end

  output
ensure
  $stdout.sync = prev_sync
end

if ARGV.length < 2
  $stderr.puts("#{$0}: error: too few arguments")
  $stderr.puts("Usage: #{$0} RESULT_FILE TEST_RUNNER OPTIONS")
  exit 1
end

result_file = ARGV.shift
runner = ARGV.shift

output = invoke_runner(runner, parse_options(ARGV))

output.rewind
File.open(result_file, 'w') {|f| f.write(output.read) }

$stdout.puts()

output.rewind
report_summarize_result(output)
