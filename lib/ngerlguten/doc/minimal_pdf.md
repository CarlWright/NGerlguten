A Mininal PDF Construction
=========================

The following code is the minimum you need to create a one page PDF
with calls to the pdf module.

###   PDF = pdf:new(),

    To create a pdf into which to put content

###    pdf:set_pagesize(PDF,a4),

To set the page size of the PDF. Other choices are letter, legal, lots of A formats and B formats.

###    pdf:set_page(PDF,1),

To set the page you are working on

###   pdf:set_font(PDF, "Victorias-Secret", 40),

To set the font to use until told different

###    pdf_lib:moveAndShow(PDF, 50, 700, "Hello Joe from Gutenburg"),

To move to position (50,700) and place the "Hello Joe from Gutenberg content"

###    Serialised = pdf:export(PDF),

To create all the content of the pdf as a string in the term Serialised

###    file:write_file("../test/eg_test6.pdf",[Serialised]),

To output the pdf content into a finished PDF file

###    pdf:delete(PDF).

To delete the PDF object and the PDF process.