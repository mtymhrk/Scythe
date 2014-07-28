#!/usr/bin/ruby
# -*- coding:utf-8 -*-

require "#{File.expand_path(File.dirname(__FILE__))}/colour_reporter"

def report_summarize_result(input)
  failed = []
  summary = []

  input.each_line do |line|
    if summary.length > 0 or line =~ /^-+$/
      summary.push(line)
    elsif line =~ /(.+:TEST\(\s*\w+\s*,\s*\w+\s*\):FAIL(:.*?)?)[.!]*\n/
      failed.push($1)
    end
  end

  summary.shift

  puts("=======================")
  puts(" Summary")
  puts("-----------------------") unless failed.empty?
  failed.each {|line| report(line) }
  puts("-----------------------")
  summary.each {|line| report(line) }
end

if $0 == __FILE__
  if ARGV.length > 0
    File.open(ARGV[0]) {|f| report_summarize_result(f) }
  else
    report_summarize_result($stdin)
  end
end
