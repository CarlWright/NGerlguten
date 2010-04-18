PDF module functions reference
===============================

##### begin_text/1 (PID)

This begins a text object in a PDF.

##### bezier/5 (PID,{X1,Y1},{X2,Y2},{X3,Y3},{X4,Y4})
##### bezier/9 (PID,X1,Y1,X2,Y2,X3,Y3,X4,Y4)

  This moves to X1,Y1 point as its start and then creates a cubic Bezier curve to X4,Y4 using the points in between as the control points. Bezier paths should be stroked/closed/filled with a separate command.

##### bezier_c/4 (PID,Point1,Point2,Point3)
This takes the current point as its start and then creates a cubic Bezier curve to Point3 using the points in between as the control points. Bezier paths should be stroked/closed/filled with a separate command.

##### bezier_v/3 (PID, Point1, Point2 )

This takes the current point as its start and then creates a cubic Bezier curve to Point2 using the current point and Point1 as the control points. Bezier paths should be stroked/closed/filled with a separate command.

##### bezier_y/3  (PID, Point1, Point3)

This takes the current point as its start and then creates a cubic Bezier curve to Point3 using the Point1 and Point3 as the control points. Bezier paths should be stroked/closed/filled with a separate command.

##### break_text/1 

This moves the text insertion pointer for a text object to the start of the next line. It moves you to the start of the next line.

##### circle/3 (PID, {X, Y}, Radius)

This strokes a circle centered at X, Y with a radius of Radius.

##### default_face/0 

It returns a tuple with the default font information.

##### delete/1  (PID)

This ends the process associated with the PID passed to it.
    
##### ellipse/3 (PID, {X, Y}, {XHeight, YHeight})

This strokes an ellipse centered at X,Y with an X-radius of XHeight and a Y-radius of YHeight.

##### end_text/1 (PID)

This ends a text object in a PDF.

##### export/1  (PID) 

This returns a tuple with {a string that represents all of the content that you've created in your pdf, page number}.

##### fontName/1 (Font)

This turns the fontName into an index 1,2,3.. The first 14 are the standard fonts

##### get_page_no/1 (PID)

This returns the number of the page you are on. 

##### get_string_width/4 (PID, FontName, FontSize, String)

This calculates the length in points of the String stroked in FontName of size FontSize.

##### grid/3 (PID,XList,YList)

Grid assumes sorted XLists and YList, minimum value first

##### image/2  (PID, FilePath)
##### image/3  (PID, FilePath, Size)
##### image/4  (PID, FilePath, {X,Y}, Size)
    
This inserts an image into a PDF. The Size can be {width, W} | {height, H} | {W,H}.
     
##### inBuiltFonts/0

This returns a list of strings that name the standard "in-built" fonts.

##### kernedtext/2 (PID, TextList)

This allows precise kerning of the text in the TextList. It has a format like ["A", 120, "W", 120, "A", 95, "Y"]. The numbers between the letters adjust the distance between the letters. The unit of measure is thousandths of a unit of text space. You can have strings or single letters between the numbers.

##### line/3 (PID, From, To)
##### line/5  (PID, X1, Y1, X2, Y2)

This draws a lines from X1, Y1 to X2, Y2. The line shape, color, etc. is defined by the graphics state. From and To are tuples like {X1, Y2}.

##### lines/2  (PID, LineList)

This draws a list of lines. The LineList has entries of values like those needed for the line function.

##### mirror_xaxis/2 (PID, Ytranslate)
##### mirror_yaxis/2  (PID, Xtranslate)

This calls the translate function to move the graphics coordinate systems as define dby the "translate" parameter and then calls "scale" to invert the selected coordinate axis with an appropriate -1 scaling.

##### move_to/2 (PID, {X, Y})

This begins a new subpath by moving to the X,Y coordinates with no connecting line segment.

##### new/0 

Creates a process to construct a PDF file. This returns the PID of the process.

##### new_page/1  (PID)

This adds current page context to PDF document and start on a new page. Note: page 1 is already created  by default and  current page set to it after creation of PDF context.

##### pagesize/1 

Select a page size from the available sizes: a0 -> a9, b0 -> b10, c5e, comm10e, dle, executive, folio, ledger, legal, or tabloid.
    
##### path/2  (PID, StrokeType)

This indicates how to process the current path. StrokeType can be close, stroke, close_stroke, fill, fill_even_odd, fill_stroke, fill_then_stroke, fill_stroke_even_odd, close_fill_stroke, close_fill_stroke_even_odd, or endpath. If don't use the verson with StrokeType, you need to call path/2 to stroke the path.

##### poly/2 (PID,Points)

This creates a path starting at the first Point and then to each following Point. Each Point in the Points lis tis a tuple {X,Y}. Poly paths should be stroked/closed/filled with a separate command.

##### rectangle/3  (PID,{X,Y}, {WX,WY})
##### rectangle/4  (PID,{X,Y}, {WX,WY}, StrokeType)
##### rectangle/6  (PID,X,Y, WX,WY, StrokeType)

This creates a rectanglar path whose basepoint is X,Y and whose dimensions are WX, WY. It is drawn according to the selected StrokeType. It can be close, stroke, close_stroke, fill, fill_even_odd, fill_stroke, fill_then_stroke, fill_stroke_even_odd, close_fill_stroke, close_fill_stroke_even_odd, or endpath. If don't use the verson with StrokeType, you need to call path/2 to stroke the path.

##### restore_state/1 

This restores the graphics state off the graphics state stack.

##### rotate/2 

Uses the angle given to rotate the graphics coordinate space. Usually you save_state before and restore_state afterward. 

##### round_rect/4  (PID,{X,Y}, {W,H}, Radius)

This draws a rounded rectangle path. Its base is at X,Y and it has a size of W by H. The corner radius is Radius.

##### round_top_rect/4  (PID,{X,Y}, {W,H}, Radius)

This draws a rounded rectangle path with only the top corners rounded. Its base is at X,Y and it has a size of W by H. The corner radius is Radius.

##### save_state/1 (PID)

This saves the graphics state on the graphics state stack.

##### scale/3 (PID, ScaleX, ScaleY)

This changes the size of the text object in the X and Y directions.

##### set_author/2 (PID, AuthorString)

Set the Author attribute of the PDF file.

##### set_char_space/2 (PID, Space)

This sets the distance between glyphs (the image of a letter/symbol). The default is zero.

##### set_dash/2
##### set_dash/3 

This changes the graphics state so that lines stroked are "solid", "dash", "dot" or "dashdot". You specify other patterns with a list of numbers expressing the pattern.
    
##### set_date/4  (PID,Year,Month,Day)

This sets the value in the Date attribute of the PDF file. 

##### set_fill_color/2  (PID, Color)
##### set_fill_color_CMYK/5  (PID,C,M,Y,K)
##### set_fill_color_RGB/4  (PID,R,G,B)

This sets the fill color in the graphics state to the color value given. "Color" is a tuple like {16#FF,16#FF,16#FF} (this is white). You can also use pdf_op:color(darkturquoise) to select a color. pdf_op has long list of colors you can select this way.


##### set_fill_gray/2  (PID, Gray)

This sets the fill color of the graphics state to the value of "Gray" given in the call. For example,  0.0 = Black 1.0 = White.

##### set_font/3 (PID, FontName, Size)

This set the font and the font size of the graphics state for the present content stream (Typically, a text object).

##### set_keywords/2

This sets the values in the Keyword attribute of the PDF file. 

##### set_line_cap/2

This changes the graphics state so that the ends of open subpaths and dashes are shaped as "flat_cap", "round_cap" or "square_cap".
    
##### set_line_join/2 (PID, choice)

This changes the graphics state so that consecutive segments of a path that connect at angles are styled as "miter_join", "round_join" or "bevel_join".

##### set_line_width/2 (PID, Width)

This sets the graphics state so that the width of lines stroked match the width.

##### set_miter_limit/2 (PID, Limit)

"When two line segments meet at a sharp angle and mitered joins are the line join style, it is possible for the miter to extend far beyond the thickness of the line strokin gthe path. TH emiter limit imposes a maximum on the ratio of the miter length to the lin ewidth. When the limit is exceeded, the join is converted from a miter to a bevel." - ADOBE Pdf Reference document

##### set_page/2 (PID, PageNo)

This moves you to a page that already exists.

##### set_pagesize/2 (PID, Size)
##### set_pagesize/3 (PID, Width, Height)

This sets the page size. You can use pdf:pagesize(a1) for "Size". Substitute another term for "a1" to get other size. The available sizes include: a0 -> a9, b0 -> b10, c5e, comm10e, dle, executive, folio, ledger, legal, or tabloid.

##### set_stroke_color/2 (PID, Color)
##### set_stroke_color_CMYK/5  (PID,C,M,Y,K)
##### set_stroke_color_RGB/4 (PID,R,G,B)

This sets the stoke color in the graphics state to the color value given. "Color" is a tuple like {16#FF,16#FF,16#FF} (this is white). You can also use pdf_op:color(darkturquoise) to select a color. pdf_op has long list of colors you can select this way.

##### set_stroke_gray/2 (PID, Gray)

This sets the stroke color of the graphics state to the value of "Gray" given in the call. For example,  0.0 = Black 1.0 = White.

##### set_subject/2

This sets the value of the SUbject attribute of the PDF file.

##### set_text_leading/2 (PID, Amount)

This sets the distance between the baselines of adjacent lines of text.

##### set_text_pos/3  (PID, X, Y)

This sets the start position to begin painting text (glyphs).
    
##### set_text_rendering/2 (PID, fill|stroke|fill_then_stroke|Integer)

This sets the text rendering mode parameter. This determines whether showing text causes the glyph outlines to be stroked, filled, used a a clipping boundary or a combination of the previous.

##### set_text_rise/2 (PID, Rise)

This attribute raises or lowers the baseline for the following text.

##### set_text_scale/2 (PID, Percentage)

This sets the horizaontal scaling of the following glyphs. 100% is normal. 50% makes them half as wide.

##### set_title/2  (PID, TitleString)

Set the Title attribute of the PDF file.

##### set_word_space/2 (PID, Spacing)

This changes the size of a space character. In horizontal writing a positive value increases the length of a space and a negative one reduces it. In vertical writing, the effect is reversed. 

##### skew/3 (PID, XScewAngle, YScewAngle)

This changes the X & Y cordinate directions from 0 degrees for X and 90 for Y. This might be used to make letters look italic.

##### text/2 (PDF, String)

This inserts the content of the String into the PDF using the graphics state and font information set at the time.
    
##### textbr/2 (PID, String)

This inserts the content of the String into the text object using the graphics state and font information set at the time. It also ends the line (breaks) and starts a new one.

##### text_rotate/2 (PID, Angle)

Uses the angle given to rotate the text (not graphic) coordinate space. Usually you save_state before and restore_state afterward. 

##### text_rotate_position/4  (PID, X, Y, Angle)

Uses the angle given to rotate the text (not graphic) coordinate space. Usually you save_state before and restore_state afterward. 

##### text_transform/7 (PID, A, B, C, D, E, F)

This A through F sequence of numbers represents any linear transformation from one coordinate system to another for text. You can "translate", "rotate", "scale" and "skew" all in one step. Hold on to your horses!

##### transform/7 (PID, A, B, C, D, E, F)

This A through F sequence of numbers represents any linear transformation from one coordinate system to another for graphics. You can "translate", "rotate", "scale" and "skew" all in one step. Hold on to your horses!

##### translate/3 (PID, X, Y)

This translates the origin of the coordinate systems in the horizon & vertical dimension respectively.

