SHELL := /bin/bash

DIALYZER_APPS = \
	erts stdlib crypto public_key inets xmerl sasl tools kernel

# some modules which use the native option will be native compiled
compile:
	./rebar -D NATIVE_COMPILE compile

doc:	compile
	./rebar skip_deps=true compile doc

eunit:	compile
	./rebar eunit

## create the script
generate: 	compile
	./rebar compile escriptize

clean:
	rm -rf `find . -name *.beam`

dialyze:	compile
	dialyzer --plt .dialyzer_plt  -pa ebin -Wno_return \
		     --apps ebin

.create_plt:
	dialyzer --no_check_plt --build_plt  --output_plt .dialyzer_plt \
		     --apps $(DIALYZER_APPS)

xref:		compile
	./rebar xref

check: xref dialyze
