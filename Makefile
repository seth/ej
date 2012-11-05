ERL ?= erl
APP := cheferl

.PHONY: deps test doc

all: deps
	@./rebar compile

DEPSOLVER_PLT=$(PWD)/.dialyzer.plt
APPS=kernel stdlib

$(DEPSOLVER_PLT):
	@dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt --apps $(APPS)

dialyzer: all $(DEPSOLVER_PLT)
	@dialyzer --plt $(DEPSOLVER_PLT) -Wunderspecs -r ebin

test:
	@./rebar eunit

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

doc:
	@./rebar doc
