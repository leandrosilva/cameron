APP_NAME := cameron

.PHONY: deps test

all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean
	rm -rf erl_crash.dump

distclean: clean
	./rebar delete-deps

test: all
	./rebar skip_deps=true eunit

docs:
	./rebar skip_deps=true doc

##
## Release targets
##
rel: deps compile
	./rebar compile generate

relclean:
	rm -rf rel/cameron

##
## Developer targets
##
stagedevrel: devrel
	$(foreach dep,$(wildcard deps/*), rm -rf dev/cameron_dev/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) dev/cameron_dev/lib;))

devrel: deps
	mkdir -p dev
	(cd rel && ../rebar generate target_dir=../dev/cameron_dev overlay_vars=vars/dev_vars.config)

devclean:
	rm -rf dev

stage: rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/cameron/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/cameron/lib;)