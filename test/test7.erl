-module(test7).
-import(pdf_op, [n2s/1]).
-import(pdf_lib, [showGrid/2, moveAndShow/4]).

-export([test/0]).

test()->
    PDF = pdf:new(),
    pdf:set_pagesize(PDF,a4),
    pdf:set_page(PDF,1),
    pdf:set_font(PDF, "Victorias-Secret", 40),
    moveAndShow(PDF, 50, 700, "Hello Joe from Gutenburg"),
    Serialised = pdf:export(PDF),
    file:write_file("test7.pdf",[Serialised]),
    pdf:delete(PDF).












