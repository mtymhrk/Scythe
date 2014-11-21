
RUBY = ruby
COMPILER = ../../build/src/scythe-compile-file
CONVERTER = scmcompile.rb
SRC_PATH = ..
HEADER_PATH = ../include/scythe

all: ${SRC_PATH}/repl_data.c ${SRC_PATH}/compiler_data.c

${SRC_PATH}/repl_data.c: ${SRC_PATH}/repl.scm ${SRC_PATH}/repl_data.c.in
	${RUBY} ${CONVERTER} ${COMPILER} $^ $@

${SRC_PATH}/compiler_data.c: ${SRC_PATH}/compiler.scm ${SRC_PATH}/compiler_data.c.in
	${RUBY} ${CONVERTER} ${COMPILER} $^ $@
