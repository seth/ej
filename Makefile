ERL ?= erl
APP := cheferl

.PHONY: deps test doc

all: deps
	@./rebar compile

dialyzer: all
	@dialyzer -Wunderspecs -r ebin

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
