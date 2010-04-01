The tests 
=========
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

### eg_tmo_test

When you run eg:tmo_test:file(). it produces a 9 page PDF from the file process.xml. The contents are some Erlang software development standards. This code produces the PDF based on the content of the XML file. Within this test program are functions to produce tables in a PDF. It produces the revision history table on the second page and it is used to format the section headings between the lines. This is a sophisticated example of using the pdf modules to format a document.
 
### tmo_doc

When you run tmo_doc:file(), you get an output file that at first glance looks the same as the results from eg_tmo_test. The internals are different in many places. 

### kd_test1

When you run kd_test1:test(), it produces a 1 page PDF. It is a commercial bill in Swedish for a some recording, I think. It does a good job of including a graphics to show the vendor's logo. It looks interesting, but it has some problems with missing font information.

### eg_test12

When you run eg_test12:test(), it produces a 4 page PDF that looks much the same as the results of eg_test1.
