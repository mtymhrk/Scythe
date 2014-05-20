
RUBY = ruby1.9
COMPILER = ../../build/src/scythe
CONVERTER = scmcompile.rb

../repl_code.h: ../repl.scm ../repl_code.h.in
	${RUBY} ${CONVERTER} ${COMPILER} $^ $@
