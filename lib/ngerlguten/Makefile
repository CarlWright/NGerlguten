ERLC_FLAGS=+nowarn_unused_vars +nowarn_unused_function
include ../../conf/include.mk

../../ebin/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o ../../ebin $<

MODS= eg_font_map eg_font_1 eg_font_2 eg_font_3 eg_font_4 eg_font_5 eg_font_6 eg_font_7 eg_font_8 eg_font_9 eg_font_10 eg_font_11 eg_font_12 eg_font_13 eg_font_14 eg_font_15 eg_font_16 eg_font_17 

EBIN_FILES=${MODS:%=../../ebin/%.beam}
all: ${EBIN_FILES}

clean:
	rm -f ${EBIN_FILES}

