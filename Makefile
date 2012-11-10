.PHONY: clean deps distclean doc dyalyzer test

# allow rebar binary to be set via environment variable
REBAR ?= rebar

all:
	@$(REBAR) compile

dialyzer: all
	@dialyzer -Wunderspecs -r ebin

test:
	@$(REBAR) eunit

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps

doc:
	@$(REBAR) doc
