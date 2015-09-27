
##===============================================================
## install 設定

install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  DESTINATION ${SCYTHE_INSTALL_DIR}
  FILES_MATCHING
  PATTERN "*.scm"
)
