all: compile

clean:
	@./rebar clean

nuke: clean
	@rm -rf deps

check: compile
	@./rebar eunit skip_deps=true

dependencies:
	@./rebar get-deps

compile: dependencies
	@./rebar compile

include install.mk
