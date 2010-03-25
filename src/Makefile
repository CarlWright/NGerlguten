ERLC_FLAGS=+nowarn_unused_vars +nowarn_unused_function
include ../conf/include.mk

ERL_OBJECTS += ../ebin/eg_hyphen_rules.beam egFontMap

all: $(ERL_OBJECTS) $(ERL_DOCUMENTS)

eg_hyphen_rules.erl: ../ebin/eg_mk_hyphen.beam ../priv/ukhyphen.tex
	 erl -noshell -pa ../ebin -s eg_mk_hyphen start -s erlang halt

../ebin/eg_hyphen_rules.beam: eg_hyphen_rules.erl
../ebin/eg_mk_hyphen.beam : eg_mk_hyphen.erl

egFontMap: ../priv/fonts/bin/Makefile

../priv/fonts/bin/Makefile: ../ebin/eg_afm.beam ../priv/fonts/font_locations
	echo "eg_afm:make(), erlang:halt()." | erl -pa ../ebin > /dev/null

clean:
	-rm -f $(ERL_OBJECTS) eg_hyphen_rules.erl
	-rm -f erl_crash.dump
