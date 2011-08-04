APP_NAME := cameron

all: compile

clear:
	@rm -f ebin/*.beam
	@rm -f erl_crash.dump
	
compile: clear
	@cp src/$(APP_NAME).app ebin/
	@erlc -pa ebin/ \
				-pa deps/ \
				-pa deps/erl_helpers/ebin/ \
				-pa deps/erlang-uuid/ebin/ \
				-pa deps/misultin/ebin/ \
				-pa deps/redo/ebin/ \
				-pa deps/erlang_syslog/ebin/ \
				-I include/ \
				-I deps/misultin/include/ \
				-o ebin/ \
				src/*.erl

compile_prod: clear
	@cp src/$(APP_NAME).app ebin/
	@erlc -D use_syslog \
				-pa ebin/ \
				-pa deps/ \
				-pa deps/erl_helpers/ebin/ \
				-pa deps/erlang-uuid/ebin/ \
				-pa deps/misultin/ebin/ \
				-pa deps/redo/ebin/ \
				-pa deps/erlang_syslog/ebin/ \
				-I include/ \
				-I deps/misultin/include/ \
				-o ebin/ \
				src/*.erl
	
compile_test: compile
	@erlc -pa ebin/ \
				-pa deps/ \
				-pa deps/erl_helpers/ebin/ \
				-pa deps/erlang-uuid/ebin/ \
				-pa deps/misultin/ebin/ \
				-pa deps/redo/ebin/ \
				-pa deps/erlang_syslog/ebin/ \
				-I include/ \
				-I deps/misultin/include/ \
				-o ebin/ \
				test/*.erl

compile_deps:
	(cd deps/erl_helpers && make)
	(cd deps/erlang_syslog && make)
	(cd deps/misultin && make)
	(cd deps/redo && make)

run_dev:
	@erl +P 100000 \
			 -sname $(APP_NAME) \
			 -s $(APP_NAME) \
			 -pa ebin/ deps/**/ebin/ \
			 -boot start_sasl -config priv/sasl/all.config \
			 -config priv/config/development \
			 -processes priv/processes/development.config

run_test: compile_test
	@erl -noshell -pa ebin/ deps/**/ebin/ \
			 -s $(APP_NAME) \
			 -run $(MODULE) test -run init stop \
			 -config priv/config/test \
			 -processes priv/processes/test.config

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

run_prod:
	@erl +P 100000 \
			 -sname $(APP_NAME) \
			 -s $(APP_NAME) \
			 -pa ebin/ deps/**/ebin/ \
			 -boot start_sasl -config priv/sasl/all.config \
			 -config priv/config/production \
			 -processes priv/processes/production.config
