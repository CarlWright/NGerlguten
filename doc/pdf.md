PDF modules functions reference
===============================


## allFonts/0

## append_stream/2 

## begin_text/1 (PID)

    This begins a text object in a PDF.

## bezier/5 

## bezier/9 

## bezier_c/4 

## bezier_v/3 

## bezier_y/3arc/5 

## break_text/1 

## circle/3 

## default_face/0 

## delete/1  (PID)

    This ends the process associated with the PID passed to it.
    
## ellipse/3 

## end_text/1 

## ensure_font_gets_loaded/2

## export/1  (PID) 

    This returns a string that represents all of the content that you've created in your pdf.

## fontName/1

## get_page_no/1

## get_string_width/4 

## grid/3 

## image/2  (PID, FilePath)
## image/3  (PID, FilePath, Size)
## image/4  (PID, FilePath, {X,Y}, Size)
    
    This inserts an image into a PDF. The Size can be {width, W} | {height, H} | {W,H}.
     
## inBuiltFonts/0

## kernedtext/2 

## line/3 

## line/5 

## lines/2

## mirror_xaxis/2 

## mirror_yaxis/2

## move_to/2 

## new/0 

    Creates a process to construct a PDF file. This returns the PID of the process.

## new_page/1 

## page_script/2 

## pagesize/1 

    Select a page size from the available sizes: a0 -> a9, b0 -> b10, c5e, comm10e, dle, executive, folio, ledger, legal, or tabloid.
    
## path/2

## poly/2 

## rectangle/3 

## rectangle/4 

## rectangle/6 

## restore_state/1 

## rotate/2 

    Uses the angle given to rotate the graphics coordinate space. Usually you save_state before and restore_state afterward. 

## round_rect/4

## save_state/1

## scale/3

## set_author/2 (PID, AuthorString)

    Set the Author attribute of the PDF file.

## set_char_space/2 

## set_dash/2
## set_dash/3 

    This changes the graphics state so that lines stroked are "solid", "dash", "dot" or "dashdot". You specify other patterns with a list of numbers expressing the pattern.
    
## set_date/4

## set_fill_color/2 

## set_fill_color_CMYK/5 

## set_fill_color_RGB/4 

## set_fill_gray/2

## set_font/3 

## set_keywords/2

## set_line_cap/2

    This changes the graphics state so that the ends of open subpaths and dashes are shaped as "flat_cap", "round_cap" or "square_cap".
    
## set_line_join/2 (PID, choice)

    This changes the graphics state so that consecutive segments of a path that connect at angles are styled as "miter_join", "round_join" or "bevel_join".

## set_line_width/2 

## set_miter_limit/2 

## set_page/2 

## set_pagesize/2 

## set_pagesize/3 

## set_stroke_color/2 

## set_stroke_color_CMYK/5

## set_stroke_color_RGB/4 

## set_stroke_gray/2 

## set_subject/2

## set_text_leading/2

## set_text_pos/3  (PID, X, Y)

    This sets the start position to begin painting text (glyphs).
    
## set_text_rendering/2

## set_text_rise/2 

## set_text_scale/2

## set_title/2  (PID, TitleString)

    Set the Title attribute of the PDF file.

## set_word_space/2 

## skew/3

## text/2 (PDF, String)

    This inserts the content of the String into the PDF using the graphics state and font information set at the time.
    
## textbr/2 

## transform/7 

## translate/3 

