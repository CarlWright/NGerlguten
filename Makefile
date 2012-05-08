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

distclean:
	@rm -r rel/$(app)

dist: compile
	@./rebar generate force=1
	@chmod 755 rel/$(app)/bin/$(app)

include install.mk
install: dist
	@cp -R rel/$(app) $(prefix)
