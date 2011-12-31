
  
ErlGuten
========

Using Erlang in typography applications
---------------------------------------

INTRODUCTION
============

  ErlGuten.  is  a system  for  hiqh quality  typesetting,
ErlGuten  is  free software.   ErlGuten  aims  to produce  typographic
quality PDF directly from XML or from a program.

   The aim of ErlGuten is to produce high quality PDF from a layout
language  or from  a program.   The ErlGuten  distribution  includes a
programming  API, so  that Erlang  programs can  produce PDF  -  and a
typesetting system for typesetting documents written in XML.

  The  name  ErlGuten  is chosen  because  the program  is
written  in  Erlang  -  the  Guten part  is  a  reference  to
Gutenberg the father of printing.

  ErlGuten is a  system for high quality typesetting,  so we take a
great  deal  of   care  when  formatting  text,  a   large  number  of
optimizations are  performed which improve the quality  of the printed
text.  Many of these optimizations are usually only found in expensive
professional type-setting programs.   We believe that WYSIWYG programs
have  destroyed the fine  art of  typesetting -  ErlGuten is  a modest
attempt to improve the situation.

  We have  chosen XML as the  input language for  it's wide appeal.
XML provides only a thin abstraction layer over the typesetting system
- so the adventurous  can use the programming interface  to ErlGuten -
to directly  produce typographic quality PDF in  real-time.  We expect
this facility  to be  useful for the  dynamic generation  of documents
from web-servers.

  In  ErlGuten  we  take  the   view  that  the  highest  level  of
abstraction  is  the layout  of  a  document  - we  are  very
concerned  that the user  can specify  the exact  position of
text on  the printed  page. At  the next level  of abstraction  we are
concerned with the typefaces that are used to format different regions
of the document.

  ErlGuten  is designed  for the  production of  large  and complex
documents with  complex layout requirements,  like newspapers
or  books.    In  ErlGuten  layout,   content,  and  document
management  are  considered separate  issues.  Layout is  template
based -  Content is  assumed to  be stored as  a large  number of
documents in  a file  system or data  base, document  management is
considered  as  a mapping  operation  which  takes  documents in  the
content  data base  and  maps  them onto  templates  to produce  hight
quality output.

  This  is normal text,  set 30  picas wide  in 12/14  Times Roman.
Many   different   typefaces  can   be   used   within  a   paragraph.
Emphasized text is set in Times-Italic.  Hyphenation uses the
TeX hyphenation algorithm.  Any of  the 35 built-in PDF typefaces with
the  same  point  size  can  be  mixed with  a  paragraph.   The  term
{person,"Joe"} is  an Erlang term which  has been typeset
in 12  point courier.  The  paragraph justification algorithm
does  proper  kerning  so,  for  example, the  word  AWAY  is
correctly kerned!  - line breaks  within a paragraph are selected by a
sophisticated global optimization technique.





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



The tests 
=========

### comcastBill

When you run comcastBill:test(), you get a two page PDF which reproduces my Comcast bill for internet services.
### eg_test1

When you run eg_test1:test(), it  produces a four page PDF (eg_test1.pdf) using the pdf module function calls. It illustrates most features of the pdf module.

### eg_test2

When you run eg_test2:test(), it produces a pretty ugly three page PDF (eg_test2.pdf) that also tests a number of features of the pdf module.

### eg_test3

When you run eg_test3:test(), it produces one page of PDF. It includes several tests of justification of text. These tests include columns of text put into boxes located on the page. In the background is a grid showing the location of the items on the page. This uses the pdf module directly , but then also uses the xml parsing modules to processing xml strings into the PDF content.

### eg_test4

When you run eg_test4:test(), it produces a two page PDF. The 1st page is an A4 layout grid with a few lines of text placed on it. The second page shows 14 examples of the same text in different fonts.

### eg_test5

When you run eg_test5:test(), it produces a page of PDF. This page shows the planning grid and on it are 8 blocks of text. The content of each block is the same, but the blocks are different sizes, different justification schemes and arranged in angular rotation.
If you run eg_test5:test(X) where is X is a number from 1 to 7 you get the tuple returned by break_richText when it processes its test paragraph with different kinds of justification and line lengths. If you run eg_test:test(8), you get the tuple returned by eg_line_break:make_partitions. This takes a sentence and shows you the places where you can hyphenate the last word in the sentence.

### eg_test6

When you run eg_test6:test(), it produces a page of PDF. This is a very simple page produced by calls to the pdf module.

### eg_test7

When you run eg_test8_test(), it produces the same output as eg_test1, but it assembles a pdf document directly by building it from pdf "objects"
 rather than going through the pdf.erl API (and its pdf process). This test mostly producess the same output as eg_test1.erl._

### eg_test8

When you run eg_test8:test(), it produces a page of PDF. This tests the eg_table module by putting size tables on the page.

### eg_test12

When you run eg_test12:test(), it produces a 4 page PDF that looks much the same as the results of eg_test1.

### eg_tmo_test

When you run eg:tmo_test:file(), it produces a 9 page PDF from the file process.xml. The contents are some Erlang software development standards. This code produces the PDF based on the content of the XML file. Within this test program are functions to produce tables in a PDF. It produces the revision history table on the second page and it is used to format the section headings between the lines. This is a sophisticated example of using the pdf modules to format a document.
 
### tmo_doc

When you run tmo_doc:file(), you get an output file that at first glance looks the same as the results from eg_tmo_test. The internals are different in many places. 

### kd_test1

When you run kd_test1:test(), it produces a 1 page PDF. It is a commercial bill in Swedish for a some recording, I think. It does a good job of including a graphics to show the vendor's logo. It looks interesting.
