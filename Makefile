ERL ?= erl
APP := uptimer

.PHONY: deps

all: rebar
	@./rebar compile


./rebar:
	wget https://github.com/rebar/rebar/wiki/rebar
	chmod +x ./rebar

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[]'


test: all
	@./rebar ct

