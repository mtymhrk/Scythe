
RUBY = ruby
COMPILER = ../../build/src/scythe-compile-file
CONVERTER = scmcompile.rb

all: ../repl_code.h ../compiler_code.h

../repl_code.h: ../repl.scm ../repl_code.h.in
	${RUBY} ${CONVERTER} ${COMPILER} $^ $@

../compiler_code.h: ../compiler.scm ../compiler_code.h.in
	${RUBY} ${CONVERTER} ${COMPILER} $^ $@
