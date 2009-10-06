all: compile test

compile:
	mkdir -p ebin
	mkdir -p test/ebin
	erl -make

clean:
	rm -rf ./coverage/*.*
	rm -rf ./ebin/*.*
	rm -rf ./test/ebin/*.*

test: compile
	erl -noshell -pa ebin -pa test/ebin -s test_suite test -s init stop

coverage: compile
	mkdir -p coverage
	erl -noshell -pa ebin -pa test/ebin -s eunit_helper run_cover -s init stop


tidy:
	erl -noshell \
		-eval 'erl_tidy:dir("./src", [{verbose,true},{test,true}])' \
		-eval 'erl_tidy:dir("./test/src", [{verbose,true},{test,true}])' \
		-s init stop
	find ./ -name \*.bak -exec rm {} \;
