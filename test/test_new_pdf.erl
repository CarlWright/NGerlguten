-module(test_new_pdf).

-export([test/0]).

%% ============================================================================

test()->
    PDF = pdf_otp:new(),
    pdf_otp:set_pagesize(PDF,letter),
    pdf_otp:set_page(PDF,1),
    pdf_otp:set_font(PDF, "Victorias-Secret", 40),
    

    pdf_otp:begin_text(PDF),
    pdf_otp:set_text_pos(PDF, 50,700),
    pdf_otp:text(PDF, "Hello Joe from Gutenburg"),
    pdf_otp:end_text(PDF),

    {Serialised, _PageNo} = pdf_otp:export(PDF),
    file:write_file("../test/test_new_pdf.pdf",[Serialised]),
    pdf_otp:delete(PDF).

