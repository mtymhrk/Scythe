
RUBY = ruby
COMPILER = ../../build/src/scythe-compile-file
CONVERTER = scmcompile.rb
HEADER_PATH = ../include/scythe

all: ${HEADER_PATH}/repl_code.h ${HEADER_PATH}/compiler_code.h

${HEADER_PATH}/repl_code.h: ../repl.scm ${HEADER_PATH}/repl_code.h.in
	${RUBY} ${CONVERTER} ${COMPILER} $^ $@

${HEADER_PATH}/compiler_code.h: ../compiler.scm ${HEADER_PATH}/compiler_code.h.in
	${RUBY} ${CONVERTER} ${COMPILER} $^ $@
