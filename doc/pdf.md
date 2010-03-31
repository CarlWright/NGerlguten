PDF modules functions reference
===============================

## begin_text/1 (PID)

    This begins a text object in a PDF.

## bezier/5 

## bezier/9 

## bezier_c/4 

## bezier_v/3 

## bezier_y/3arc/5 

## break_text/1 

    This moves the text insertion pointer for a text object to the start of the next line. It moves you to the start of the next line.

## circle/3 (PID, {X, Y}, Radius)

    This strokes a circle centered at X, Y with a radius of Radius.

## default_face/0 

    It returns a tuple with the default font information.

## delete/1  (PID)

    This ends the process associated with the PID passed to it.
    
## ellipse/3 (PID, {X, Y}, {XHeight, YHeight})

    This strokes an ellipse centered at X,Y with an X-radius of XHeight and a Y-radius of YHeight.

## end_text/1 (PID)

    This ends a text object in a PDF.

## export/1  (PID) 

    This returns a string that represents all of the content that you've created in your pdf.

## fontName/1 (Font)

    This turns the fontName into an index 1,2,3.. The first 14 are the standard fonts

## get_page_no/1 (PID)

    This returns the number of the page you are on. 

## get_string_width/4 (PID, FontName, FontSize, String)

    This calculates the length in points of the String stroked in FontName of size FontSize.

## grid/3 (PID,XList,YList)

    grid assumes sorted XLists and YList, minimum value first

## image/2  (PID, FilePath)
## image/3  (PID, FilePath, Size)
## image/4  (PID, FilePath, {X,Y}, Size)
    
    This inserts an image into a PDF. The Size can be {width, W} | {height, H} | {W,H}.
     
## inBuiltFonts/0

    This returns a list of strings that name the standard "in-built" fonts.

## kernedtext/2 (PID, TextList)

    This allows precise kerning of the text in the TextList. It has a format like ["A", 120, "W", 120, "A", 95, "Y"]. The numbers between the letters adjust the distance between the letters. The unit of measure is thousandths of a unit of text space. You can have strings or single letters between the numbers.

## line/3 

## line/5 

## lines/2

## mirror_xaxis/2 

## mirror_yaxis/2

## move_to/2 

## new/0 

    Creates a process to construct a PDF file. This returns the PID of the process.

## new_page/1  (PID)

    This adds current page context to PDF document and start on a new page. Note: page 1 is already created  by default and  current page set to it after creation of PDF context.

## pagesize/1 

    Select a page size from the available sizes: a0 -> a9, b0 -> b10, c5e, comm10e, dle, executive, folio, ledger, legal, or tabloid.
    
## path/2  (PID, StrokeType)

    This indicates how to process the current path. StrokeType can be close, stroke, close_stroke, fill, fill_even_odd, fill_stroke, fill_then_stroke, fill_stroke_even_odd, close_fill_stroke, close_fill_stroke_even_odd, or endpath. If don't use the verson with StrokeType, you need to call path/2 to stroke the path.

## poly/2 

## rectangle/3  (PID,{X,Y}, {WX,WY})
## rectangle/4  (PID,{X,Y}, {WX,WY}, StrokeType)
## rectangle/6  (PID,X,Y, WX,WY, StrokeType)

    This creates a rectanglar path whose basepoint is X,Y and whose dimensions are WX, WY. It is drawn according to the selected StrokeType. It can be close, stroke, close_stroke, fill, fill_even_odd, fill_stroke, fill_then_stroke, fill_stroke_even_odd, close_fill_stroke, close_fill_stroke_even_odd, or endpath. If don't use the verson with StrokeType, you need to call path/2 to stroke the path.

## restore_state/1 

    This restore the graphics state off the graphics state stack.

## rotate/2 

    Uses the angle given to rotate the graphics coordinate space. Usually you save_state before and restore_state afterward. 

## round_rect/4  (PID,{X,Y}, {W,H}, Radius)

    This draws a rounded rectangle path. Its base is at X,Y and it has a size of W by H. The corner radius is Radius.

## save_state/1 (PID)

    This saves the graphics state on the graphics state stack.

## scale/3 (PID, ScaleX, ScaleY)

    This changes the size of the text object in the X and Y directions.

## set_author/2 (PID, AuthorString)

    Set the Author attribute of the PDF file.

## set_char_space/2 (PID, Space)

    This sets the distance between glyphs (the image of a letter/symbol). The default is zero.

## set_dash/2
## set_dash/3 

    This changes the graphics state so that lines stroked are "solid", "dash", "dot" or "dashdot". You specify other patterns with a list of numbers expressing the pattern.
    
## set_date/4

## set_fill_color/2 

## set_fill_color_CMYK/5 

## set_fill_color_RGB/4 

## set_fill_gray/2

## set_font/3 (PID, FontName, Size)

    This set the font and the font size of the graphics state for the present content stream (Typically, a text object).

## set_keywords/2

    This sets the values in the Keyword attribute of the PDF file. 

## set_line_cap/2

    This changes the graphics state so that the ends of open subpaths and dashes are shaped as "flat_cap", "round_cap" or "square_cap".
    
## set_line_join/2 (PID, choice)

    This changes the graphics state so that consecutive segments of a path that connect at angles are styled as "miter_join", "round_join" or "bevel_join".

## set_line_width/2 (PID, Width)

    This sets the graphics state so that the width of lines stroked match the width.

## set_miter_limit/2 (PID, Limit)

    "When two line segments meet at a sharp angle and mitered joins are the line join style, it is possible for the miter to extend far beyond the thickness of the line strokin gthe path. TH emiter limit imposes a maximum on the ratio of the miter length to the lin ewidth. When the limit is exceeded, the join is converted from a miter to a bevel." - ADOBE Pdf Reference document

## set_page/2 (PID, PageNo)

    This moves you to a page that already exists.

## set_pagesize/2 

## set_pagesize/3 

## set_stroke_color/2 

## set_stroke_color_CMYK/5

## set_stroke_color_RGB/4 

## set_stroke_gray/2 

## set_subject/2

    This sets the value of the SUbject attribute of the PDF file.

## set_text_leading/2 (PID, Amount)

    This sets the distance between the baselines of adjacent lines of text.

## set_text_pos/3  (PID, X, Y)

    This sets the start position to begin painting text (glyphs).
    
## set_text_rendering/2 (PID, fill|stroke|fill_then_stroke|Integer)

    This sets the text rendering mode parameter. This determines whether showing text causes the glyph outlines to be stroked, filled, used a a clipping boundary or a combination of the previous.

## set_text_rise/2 (PID, Rise)

    This attribute raises or lowers the baseline for the following text.

## set_text_scale/2 (PID, Percentage)

    This sets the horizaontal scaling of the following glyphs. 100% is normal. 50% makes them half as wide.

## set_title/2  (PID, TitleString)

    Set the Title attribute of the PDF file.

## set_word_space/2 (PID, Spacing)

    This changes the size of a space character. In horizontal writing a positive value increases the length of a space and a negative one reduces it. In vertical writing, the effect is reversed. 

## skew/3 (PID, XScewAngle, YScewAngle)

    This changes the X & Y cordinate directions from 0 degrees for X and 90 for Y. This might be used to make letters look italic.

## text/2 (PDF, String)

    This inserts the content of the String into the PDF using the graphics state and font information set at the time.
    
## textbr/2 (PID, String)

    This inserts the content of the String into the text object using the graphics state and font information set at the time. It also ends the line (breaks) and starts a new one.

## transform/7 

## translate/3 

