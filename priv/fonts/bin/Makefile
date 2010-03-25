ERLC_FLAGS=+nowarn_unused_vars +nowarn_unused_function
include ../../../conf/include.mk

../../../ebin/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o ../../../ebin $<

MODS= egFontMap egFont1 egFont2 egFont3 egFont4 egFont5 egFont6 egFont7 egFont8 egFont9 egFont10 egFont11 egFont12 egFont13 egFont14 

EBIN_FILES=${MODS:%=../../../ebin/%.beam}
all: ${EBIN_FILES}

clean:
	rm -f ${EBIN_FILES}

