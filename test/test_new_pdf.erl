-module(test_new_pdf).

-export([test/0]).

%% ============================================================================

test()->
    PDF = otp_pdf:new(),
    otp_pdf:set_pagesize(PDF,letter),
    otp_pdf:set_page(PDF,1),
    otp_pdf:set_font(PDF, "Victorias-Secret", 40),
    otp_pdf_lib:moveAndShow(PDF, 50, 700, "Hello Joe from Gutenburg"),

    {Serialised, _PageNo} = otp_pdf:export(PDF),
    file:write_file("../test/eg_test6.pdf",[Serialised]),
    otp_pdf:delete(PDF).

