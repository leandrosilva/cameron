APP_NAME := cameron

all: compile
	
compile: clear
	@cp src/$(APP_NAME).app ebin/
	@erlc -pa ebin/ \
	      -pa deps/ \
	      -pa deps/misultin-0.6.2/ebin/ \
	      -pa deps/amqp_client-2.5.0/ebin/ \
	      -pa deps/rabbit_common-2.5.0/ebin/ \
	      -pa deps/erl_helpers/ebin/ \
	      -pa deps/erlang-uuid/ebin/ \
	      -o ebin/ \
	      src/*.erl
	
compile_test: compile
	@erlc -pa ebin/ \
	      -pa deps/ \
	      -pa deps/misultin-0.6.2/ebin/ \
	      -pa deps/amqp_client-2.5.0/ebin/ \
	      -pa deps/rabbit_common-2.5.0/ebin/ \
	      -pa deps/erl_helpers/ebin/ \
	      -pa deps/erlang-uuid/ebin/ \
	      -o ebin/ \
	      test/*.erl

run:
	@erl -pa ebin/ deps/**/ebin/ -sname $(APP_NAME) -s $(APP_NAME) \
	     -config priv/config/production

run_dev:
	@erl -pa ebin/ deps/**/ebin/ -sname $(APP_NAME) \
	     -boot start_sasl -s $(APP_NAME) \
	     -config priv/config/development

run_test: compile_test
	@erl -noshell -pa ebin/ deps/**/ebin/ -s $(APP_NAME) \
	     -run $(MODULE) test -run init stop \
	     -config priv/config/test

run_all_tests: compile_test
	@for fullfilename in `find ./test -name "*_test.erl"`; do \
		filename=$$(basename $$fullfilename); \
		module=$${filename%%.*}; \
		echo ; \
		echo running: $$module; \
		erl -noshell -pa ebin/ deps/**/ebin/ -s $(APP_NAME) \
		    -run $$module test -run init stop \
		    -config priv/config/test; \
	done

clear:
	@rm -f ebin/*.beam
	@rm -f erl_crash.dump
