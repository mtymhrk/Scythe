
RUBY = ruby
COMPILER = ../../build/src/scythe-compile-file
CONVERTER = scmcompile.rb
SRC_PATH = ..
HEADER_PATH = ../include/scythe

all: ${SRC_PATH}/repl_data.c ${SRC_PATH}/compile_data.c ${SRC_PATH}/macro_data.c \
     ${SRC_PATH}/dynamic-env_data.c

${SRC_PATH}/repl_data.c: ${SRC_PATH}/repl.scm ${SRC_PATH}/repl_data.c.in
	${RUBY} ${CONVERTER} ${COMPILER} $^ $@

${SRC_PATH}/compile_data.c: ${SRC_PATH}/compile.scm ${SRC_PATH}/compile_data.c.in
	${RUBY} ${CONVERTER} ${COMPILER} $^ $@

${SRC_PATH}/macro_data.c: ${SRC_PATH}/macro.scm ${SRC_PATH}/macro_data.c.in
	${RUBY} ${CONVERTER} ${COMPILER} $^ $@

${SRC_PATH}/dynamic-env_data.c: ${SRC_PATH}/dynamic-env.scm ${SRC_PATH}/dynamic-env_data.c.in
	${RUBY} ${CONVERTER} ${COMPILER} $^ $@
