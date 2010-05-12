%%==========================================================================
%% Copyright (C) 2003 Joe Armstrong
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%% 
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%==========================================================================

-record(env,	{pdf,             % PDF process
		 currentBox,      % name of currentBox
		 template,        % current template (Module Name)
		 page,            % current page
		 dict,            % dictionary with keys{free,Page,Box}
				              %  {initialsed, Page, Template}
		 tagMap           % current TagMap (function of the box)
		 }).

-record(afm2, {baseFont,    % FontName
	       firstChar,   % first char in widths table
	       lastChar,    % last char in Widths table
	       widths,      % the widths table (0=undefined)
	       kernPairs,   % [{{C1,C2},K}]
	       ascender,    % num
	       capHeight,   % num
	       descender,   % num
	       flags,       % num
	       fontBBox,    % {int,int,int,int}
	       italicAngle, % num 
	       stemV,       % num
	       xHeight}).   % num
 
-record(info, {creator,creationDate, producer, author, title, subject,
	       keywords}). % fields in the header of a PDF 

-record(pdfContext, {
	  info,
	  fonts,
	  font_handler,
	  images=[],    %% a dictionary of images
	  currentpage,
	  pages=[],
	  scripts=[],
	  mediabox,
	  procset={undefined,undefined}  %% { imageb,imagec }
	 }).


-record(image,{ alias,
                width,
                height,
                mask        % image color key masking color values
                }).

-record(box,{name=default,  %name of the box
       x=10,                % X coordinate of top left hand corner of box
	     y=800,               % Y coordinate of top left hand corner of box
	     width=210,           % Width in points (72 point=1 inch)
	     leading=12,          % leading in points
	     lines=1,             % # lines in box
	     next=none,           % next= none | {DeltaPage, Tag} 
	     free=1,              % first free line in the box
	     grid=false,          % plot a grid
	     continue=false,       
	     fontSize=12,          % default font for box
       justify=justified,         % text justification scheme (justified, centered, right_justified, left_justified)
	     bg=default           % background color in box= default | {R,G,B}
	    }).
	    
-record(object, {
      name=default,       % name of the object
      paraIndent=0        % paragraph indentation
      }).
      
-record(tagMap, {
     name=default,        % Tag name
		 font="Times-Roman",  % Font name
		 color=default,       % default | {R,G,B}
		 size=11,             % font size
		 voff=0,              % vertical offset
		 break=true           % can do line breaks in blanks
		}).   


-record(face, {font, 
               pointSize, 
               vOffset, 
               color, 
               breakable}).

% This is used to pass font choices for different tags into eg_table
-record(table,
    { def_font="Times-Roman",     % Default font choice
      em_font="Times-Bold",       % font for <em> tag 
      b_font="Times-Bold",        % font for <b> tag
      code_font="Courier-Bold"}). % font for <code> tag