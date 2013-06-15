#!/usr/binruby
# -*- coding:utf-8 -*-

require "#{File.expand_path(File.dirname(__FILE__))}/colour_reporter"

if $0 == __FILE__
  if ARGV.length < 2
    $stderr.puts("#{$0}: error: too few arguments")
    $stderr.puts("Usage: #{$0} TEST_RUNNER RESULT_FILE")
    exit 1
  end

  runner = ARGV[0]
  result_file = ARGV[1]

  outputs = []
  IO.popen(runner) do |pipe|
    pipe.each_line do |line|
      report(line)
      outputs.push(line)
    end
  end

  result_file += (outputs[-1] =~ /OK$/) ? '.testpass' : '.testfile'
  File.open(result_file, 'w') {|f| outputs.each {|l| f.print(l) }}
end
