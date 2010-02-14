.SUFFIXES:  .beam .hrl .erl .html .ehtml

.erl.beam: 
	$(ERLC) -W $<

.ehtml.html:
	ehtml2html $<

ERLC = erlc
MODS= 	erlguten\
	erlguten_afm\
	erlguten_font_server\
	erlguten_geometry\
	erlguten_hyphen_rules\
	erlguten_hyphenate\
	erlguten_line_break\
	erlguten_lines2pdf\
	erlguten_mk_hyphen\
	erlguten_normalise_xml\
	erlguten_para_break\
	erlguten_pdf_analyse\
	erlguten_pdf_assemble\
	erlguten_pdf_export\
	erlguten_test1\
	erlguten_test2\
	erlguten_test3\
	erlguten_test4\
	erlguten_xml_lite\
	erlguten_xml_tokenise\
	pdf

all: erlguten_test1.pdf erlguten_test2.pdf erlguten_test3.pdf\
	erlguten_test4.pdf test1.pdf test2.pdf\
	${MODS:%=%.beam}

publish:
	make
	cp erlguten.html test1.pdf erlguten_test1.pdf erlguten_test4.pdf\
	   erlguten_test1.erl erlguten_test4.erl test1.xml  galley_001.gal\
	   /home/joe/public_html/erlguten-2.1
	make pack
	cp ../erlguten-2.1.tgz /home/joe/public_html/erlguten-2.1


pack:
	make clean
	cd ..; jpack erlguten-2.1

test1.pdf: test1.xml galley_001.gal ${MODS:%=%.beam}
	./erlguten test1.xml

test2.pdf: test2.xml galley_002.gal ${MODS:%=%.beam}
	./erlguten test2.xml

erlguten_test1.pdf: erlguten_test1.beam  ${MODS:%=%.beam}
	erl -pa `pwd` -s erlguten_test1 test -s erlang halt

erlguten_test2.pdf: erlguten_test2.beam ${MODS:%=%.beam}
	erl -pa `pwd` -s erlguten_test2 test -s erlang halt

erlguten_test3.pdf: erlguten_test3.beam  ${MODS:%=%.beam}
	erl -pa `pwd` -s erlguten_test3 test -s erlang halt

erlguten_test4.pdf: erlguten_test4.beam  ${MODS:%=%.beam}
	erl -pa `pwd` -s erlguten_test4 test -s erlang halt

erlguten_hyphen_rules.erl: erlguten_mk_hyphen.beam ukhyphen.tex
	echo "erlguten_mk_hyphen:start(), erlang:halt()." | erl

clean:
	rm -f *.beam test1.pdf test2.pdf erlguten_hyphen_rules.erl erlguten_test?.pdf 
	rm -f erl_crash.dump





