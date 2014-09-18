
function(dump_build_info src dst)
  set(out ${dst}/build-info.yml)

  function(format_list result)
    list(REMOVE_AT ARGV 0)
    set(x "")
    foreach(i ${ARGV})
      set(x "${x}\n  - ${i}")
    endforeach(i)
    set(${result} ${x} PARENT_SCOPE)
  endfunction()

  file(WRITE ${out} "")

  file(APPEND ${out} "compiler: ${CMAKE_C_COMPILER}\n")

  ## CMAKE_C_FLAGS の値から -iquote フラグとそれ以外を、それぞれ quoted_paths、
  ## flags 変数に分離
  set(quoted_paths "")
  set(found "FALSE")
  string(REGEX REPLACE " +" ";" flags ${CMAKE_C_FLAGS})
  foreach(f ${flags})
    if(found)
      list(REMOVE_ITEM flags ${f})
      list(APPEND quoted_paths ${f})
      set(found "FALSE")
      set(f "XXX")
    endif(found)

    string(COMPARE EQUAL "-iquote" ${f} m)
    if(m)
      set(found "TRUE")
      list(REMOVE_ITEM flags ${f})
    endif(m)

    string(REGEX MATCH "-iquote.+" m ${f})
    if(m)
      list(REMOVE_ITEM flags ${m})
      string(REGEX REPLACE "-iquote(.+)" "\\1" r ${m})
      list(APPEND quote_paths ${r})
    endif(m)
  endforeach(f)

  format_list(val ${flags})
  file(APPEND ${out} "flags: ${val}\n")

  format_list(val ${quoted_paths})
  file(APPEND ${out} "quoted-include-paths: ${val}\n")

  get_property(paths DIRECTORY ${src} PROPERTY INCLUDE_DIRECTORIES)
  format_list(val ${paths})
  file(APPEND ${out} "include-paths: ${val}\n")

  get_property(paths DIRECTORY ${src} PROPERTY COMPILE_DEFINITIONS)
  format_list(val ${paths})
  file(APPEND ${out} "definitions: ${val}\n")
endfunction()
