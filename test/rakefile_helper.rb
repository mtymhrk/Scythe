# -*- coding:utf-8 -*-
require 'yaml'
require 'fileutils'
require HERE+'unity/auto/unity_test_summary'
require HERE+'unity/auto/generate_test_runner'
require HERE+'unity/auto/generate_unity_fixture_runner'
require HERE+'unity/auto/colour_reporter'

module RakefileHelpers

  C_EXTENSION = '.c'

  def load_configuration(config_file)
    $cfg_file = "unity/targets/#{config_file}"
    $cfg = YAML.load(File.read($cfg_file))
  end

  def configure_clean
    CLEAN.include($cfg['compiler']['build_path'] + '*.*') unless $cfg['compiler']['build_path'].nil?
  end

  def configure_toolchain(config_file=DEFAULT_CONFIG_FILE)
    config_file += '.yml' unless config_file =~ /\.yml$/
    load_configuration(config_file)
    configure_clean
  end

  def get_unit_test_files
    path = $cfg['compiler']['unit_tests_path'] + 'test_*' + C_EXTENSION
    path.gsub!(/\\/, '/')
    FileList.new(path)
  end

  def get_local_include_dirs
    include_dirs = $cfg['compiler']['includes']['items'].dup
    include_dirs.delete_if {|dir| dir.is_a?(Array)}
    return include_dirs
  end

  def extract_headers(filename)
    includes = []
    lines = File.readlines(filename)
    lines.each do |line|
      m = line.match(/^\s*#include\s+\"\s*(.+\.[hH])\s*\"/)
      if not m.nil?
        includes << m[1]
      end
    end
    return includes
  end

  def find_source_file(header, paths)
    paths.each do |dir|
      src_file = dir + header.ext(C_EXTENSION)
      if (File.exists?(src_file))
        return src_file
      end
    end
    return nil
  end

  def tackit(strings)
    if strings.is_a?(Array)
      result = "\"#{strings.join}\""
    else
      result = strings
    end
    return result
  end

  def squash(prefix, items)
    result = ''
    items.each { |item| result += " #{prefix}#{tackit(item)}" }
    return result
  end

  def build_compiler_fields
    command  = tackit($cfg['compiler']['path'])
    if $cfg['compiler']['defines']['items'].nil?
      defines  = ''
    else
      defines  = squash($cfg['compiler']['defines']['prefix'], $cfg['compiler']['defines']['items'])
    end
    options  = squash('', $cfg['compiler']['options'])
    includes = squash($cfg['compiler']['includes']['prefix'], $cfg['compiler']['includes']['items'])
    includes = includes.gsub(/\\ /, ' ').gsub(/\\\"/, '"').gsub(/\\$/, '') # Remove trailing slashes (for IAR)
    return {:command => command, :defines => defines, :options => options, :includes => includes}
  end

  def compile(file, defines=[])
    compiler = build_compiler_fields
    cmd_str  = "#{compiler[:command]}#{compiler[:defines]}#{compiler[:options]}#{compiler[:includes]} #{file} " +
               "#{$cfg['compiler']['object_files']['prefix']}#{$cfg['compiler']['object_files']['destination']}"
    obj_file = "#{File.basename(file, C_EXTENSION)}#{$cfg['compiler']['object_files']['extension']}"
    execute(cmd_str + obj_file)
    return obj_file
  end

  def build_linker_fields
    command  = tackit($cfg['linker']['path'])
    if $cfg['linker']['options'].nil?
      options  = ''
    else
      options  = squash('', $cfg['linker']['options'])
    end
    if ($cfg['linker']['includes'].nil? || $cfg['linker']['includes']['items'].nil?)
      includes = ''
    else
      includes = squash($cfg['linker']['includes']['prefix'], $cfg['linker']['includes']['items'])
    end
    includes = includes.gsub(/\\ /, ' ').gsub(/\\\"/, '"').gsub(/\\$/, '') # Remove trailing slashes (for IAR)
    return {:command => command, :options => options, :includes => includes}
  end

  def link_it(exe_name, obj_list)
    linker = build_linker_fields
    cmd_str = "#{linker[:command]}#{linker[:options]}#{linker[:includes]} " +
      (obj_list.map{|obj|"#{$cfg['linker']['object_files']['path']}#{obj} "}).join +
      $cfg['linker']['bin_files']['prefix'] + ' ' +
      $cfg['linker']['bin_files']['destination'] +
      exe_name + $cfg['linker']['bin_files']['extension']
    execute(cmd_str)
  end

  def build_simulator_fields
    return nil if $cfg['simulator'].nil?
    if $cfg['simulator']['path'].nil?
      command = ''
    else
      command = (tackit($cfg['simulator']['path']) + ' ')
    end
    if $cfg['simulator']['pre_support'].nil?
      pre_support = ''
    else
      pre_support = squash('', $cfg['simulator']['pre_support'])
    end
    if $cfg['simulator']['post_support'].nil?
      post_support = ''
    else
      post_support = squash('', $cfg['simulator']['post_support'])
    end
    return {:command => command, :pre_support => pre_support, :post_support => post_support}
  end

  def execute(command_string, verbose=true, raise_on_fail=true)
    report command_string
    output = `#{command_string}`.chomp
    report(output) if (verbose && !output.nil? && (output.length > 0))
    if (($?.exitstatus != 0) and (raise_on_fail))
      raise "Command failed. (Returned #{$?.exitstatus})"
    end
    return output
  end

  def report_summary
    summary = UnityTestSummary.new
    summary.set_root_path(HERE)
    results_glob = "#{$cfg['compiler']['build_path']}*.test*"
    results_glob.gsub!(/\\/, '/')
    results = Dir[results_glob]
    summary.set_targets(results)
    summary.run
    fail_out "FAIL: There were failures" if (summary.failures > 0)
  end
  
  def run_tests(test_files)
    
    report 'Running system tests...'
    
    # Tack on TEST define for compiling unit tests
    load_configuration($cfg_file)
    test_defines = ['TEST']
    $cfg['compiler']['defines']['items'] = [] if $cfg['compiler']['defines']['items'].nil?
    $cfg['compiler']['defines']['items'] << 'TEST'
    
    include_dirs = get_local_include_dirs
    
    # Build and execute each unit test
    test_files.each do |test|
      obj_list = []
      
      # Detect dependencies and build required required modules
      extract_headers(test).each do |header|
        # Compile corresponding source file if it exists
        src_file = find_source_file(header, include_dirs)
        if !src_file.nil?
          obj_list << compile(src_file, test_defines)
        end
      end
      
      # Build the test runner (generate if configured to do so)
      test_base = File.basename(test, C_EXTENSION)
      runner_name = test_base + '_Runner.c'
      if $cfg['compiler']['runner_path'].nil?
        runner_path = $cfg['compiler']['build_path'] + runner_name
        test_gen = UnityTestRunnerGenerator.new($cfg_file)
        test_gen.run(test, runner_path)
      else
        runner_path = $cfg['compiler']['runner_path'] + runner_name
      end

      obj_list << compile(runner_path, test_defines)
      
      # Build the test module
      obj_list << compile(test, test_defines)
      
      # Link the test executable
      link_it(test_base, obj_list)
      
      # Execute unit test and generate results file
      simulator = build_simulator_fields
      executable = $cfg['linker']['bin_files']['destination'] + test_base + $cfg['linker']['bin_files']['extension']
      if simulator.nil?
        cmd_str = executable
      else
        cmd_str = "#{simulator[:command]} #{simulator[:pre_support]} #{executable} #{simulator[:post_support]}"
      end
      output = execute(cmd_str, true, false)
      test_results = $cfg['compiler']['build_path'] + test_base
      if output.match(/OK$/m).nil?
        test_results += '.testfail'
      else
        test_results += '.testpass'
      end
      File.open(test_results, 'w') { |f| f.print output }
    end
  end

  def build_application(main)

    report "Building application..."

    obj_list = []
    load_configuration($cfg_file)
    main_path = $cfg['compiler']['source_path'] + main + C_EXTENSION

    # Detect dependencies and build required required modules
    include_dirs = get_local_include_dirs
    extract_headers(main_path).each do |header|
      src_file = find_source_file(header, include_dirs)
      if !src_file.nil?
        obj_list << compile(src_file)
      end
    end

    # Build the main source file
    main_base = File.basename(main_path, C_EXTENSION)
    obj_list << compile(main_path)

    # Create the executable
    link_it(main_base, obj_list)
  end

  def fail_out(msg)
    puts msg
    exit(-1)
  end


###########################################################################
# Scythe プロジェクト用
###########################################################################

  def replace_postfix(path, from, to)
    path.sub(/#{Regexp.escape(from)}\Z/, to)
  end

  def get_unit_test_target_objs
    `make -s -C #{$cfg['compiler']['source_path']} test_list_objs`.
      chomp.
      split(/\s+/).
      map {|o| File.join($cfg['compiler']['source_path'], o) }
  end

  def get_unit_test_case_files
    `make -s -C #{$cfg['compiler']['unit_tests_path']} test_list_files`.
      chomp.
      split(/\s+/).
      map {|f| File.join($cfg['compiler']['unit_tests_path'], f) }
  end

  def get_unit_test_case_objs
    `make -s -C #{$cfg['compiler']['unit_tests_path']} test_list_objs`.
      chomp.
      split(/\s+/).
      map {|o| File.join($cfg['compiler']['unit_tests_path'], o)}
  end

  def get_unity_objs
    `make -s -C #{$cfg['compiler']['unity_src_path']} test_list_objs`.
      chomp.
      split(/\s+/).
      map {|o| File.join($cfg['compiler']['unity_src_path'], o)}
  end

  def compile_source_files
    execute("make -C #{$cfg['compiler']['source_path']} test_build 2>&1")
  end

  def clean_source
    execute("make -C #{$cfg['compiler']['source_path']} clean 2>&1")
  end

  def make_test_case_depend_file
    execute("make -C #{$cfg['compiler']['unit_tests_path']} depend 2>&1")
  end

  def compile_test_case_file(path)
    path = replace_postfix(path,
                           C_EXTENSION,
                           $cfg['compiler']['object_files']['extension'])
    file = File.basename(path)

    execute("make -C #{$cfg['compiler']['unit_tests_path']} #{file} 2>&1")

    path
  end

  def compile_test_case_file_all
    execute("make -C #{$cfg['compiler']['unit_tests_path']} 2>&1")
  end

  def clean_test_case
    execute("make -C #{$cfg['compiler']['unit_tests_path']} clean 2>&1")
  end

  def compile_unity
    execute("make -C #{$cfg['compiler']['unity_src_path']} test_build 2>&1")
  end

  def clean_unity
    execute("make -C #{$cfg['compiler']['unity_src_path']} clean 2>&1")
  end

  def test_runner_file_path(test)
    test_base = File.basename(test, C_EXTENSION)
    runner_name = test_base + '_runner.c'
    if $cfg['compiler']['runner_path'].nil?
      File.join($cfg['compiler']['build_path'], runner_name)
    else
      File.join($cfg['compiler']['runner_path'], runner_name)
    end
  end

  def build_test_runner_file(test)
    runner_path = test_runner_file_path(test)
    if $cfg['compiler']['runner_path'].nil?
#      test_gen = UnityTestRunnerGenerator.new($cfg_file)
      test_gen = UnityFixtureRunnerGenerator.new
      test_gen.run([ test ], runner_path)
    end
    runner_path
  end

  def build_test_runner_file_bundle_all(tests, runner_path)
    if $cfg['compiler']['runner_path'].nil?
      test_gen = UnityFixtureRunnerGenerator.new
      test_gen.run(tests, runner_path)
      runner_path
    else
      $cfg['compiler']['runner_path'].nil?
    end
  end

  def compile_test_runner_file(path)
    compile(path, [])

    replace_postfix(path,
                    C_EXTENSION,
                    $cfg['compiler']['object_files']['extension'])
  end

  def run_test_case(test_base)
    simulator = build_simulator_fields
    executable = File.join($cfg['linker']['bin_files']['destination'],
                           test_base + $cfg['linker']['bin_files']['extension'])
    if simulator.nil?
      cmd_str = executable
    else
      cmd_str = "#{simulator[:command]} #{simulator[:pre_support]} #{executable} #{simulator[:post_support]}"
    end

    output = execute(cmd_str, true, false)

    test_results = File.join($cfg['compiler']['build_path'], test_base)
    if output.match(/OK$/m).nil?
      test_results += '.testfail'
    else
      test_results += '.testpass'
    end
    File.open(test_results, 'w') { |f| f.print output }
  end

  # テストファイル毎に runner ファイルと実行ファイルを作成してテストを走らせ
  # るメッソド
  def run_test_cases_individually
    # テスト対象のオブジェクトファイルと Unity のオブジェクトファイルの一覧
    # を取得
    objs = get_unit_test_target_objs + get_unity_objs

    # テストコードの Makefile.depend ファイルを更新
    make_test_case_depend_file

    # Unity をコンパイル
    compile_unity

    get_unit_test_case_files.each do |test|
      test_base = File.basename(test, C_EXTENSION)

      obj_list = objs.dup

      # テストコードをコンパイルし、そのオブジェクトファイルを一覧に追加
      obj_list << compile_test_case_file(test)

      # テストコードの runner コードを作成
      runner = build_test_runner_file(test)

      # runner コードをコンパイルし、そのオブジェクトファイルを一覧に追加
      obj_list << compile_test_runner_file(runner)

      # 全てのオブジェクトファイルをリンクして実行ファイルを作成
      link_it(test_base, obj_list)

      # 実行ファイルを実行
      run_test_case(test_base)
    end
  end

  # 全てのテストをまとめて実行する runner ファイルと実行ファイルを 1 つ作成し
  # てテストを走らせるメッソド
  def run_test_cases_jointly
    test_cmd_base = 'test'

    # テスト対象のオブジェクトファイル、テストコードのオブジェクトファイル、
    # Unity のオブジェクトファイルの一覧を取得
    obj_list = get_unit_test_target_objs
    obj_list += get_unit_test_case_objs
    obj_list += get_unity_objs

    # テストコードの Makefile.depend ファイルを更新
    make_test_case_depend_file

    # Unity をコンパイル
    compile_unity

    # テストコードを全てコンパイル
    compile_test_case_file_all

    # テストコードのファイル一覧を取得
    test_files = get_unit_test_case_files

    # runner コードのファイル名の生成
    runner_path = test_runner_file_path(test_cmd_base)

    # テストコードをまとめて全て実行させる runner コードを作成
    build_test_runner_file_bundle_all(test_files, runner_path)

    # runner コードをコンパイルし、そのオブジェクトファイルを一覧に追加
    obj_list << compile_test_runner_file(runner_path)

    # 全てのオブジェクトファイルをリンクして実行ファイルを作成
    link_it(test_cmd_base, obj_list)

    # 実行ファイルを実行
    run_test_case(test_cmd_base)
  end

end
