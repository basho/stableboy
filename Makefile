.PHONY: deps

all: deps compile
	./rebar escriptize

deps:
	./rebar get-deps

compile: deps
	./rebar compile

clean:
	@./rebar clean

distclean: clean
	@rm -rf stableboy ebin deps
