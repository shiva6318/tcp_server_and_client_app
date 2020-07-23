all: clean compile
	@ ./rebar3 release

compile:
	@ ./rebar3 compile

clean:
	@ ./rebar3 clean
	@ rm -rf _build/default/rel
	
distclean: clean
	- rm -rf _build/default
