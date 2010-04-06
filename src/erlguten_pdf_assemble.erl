%%======================================================================
%% erlguten_pdf_assemble.erl - PDF assembler
%%----------------------------------------------------------------------
%% Copyright (C) 2003 Joe Armstrong, Mikael Karlsson 
%%
%%   General Terms
%%
%%   Erlguten  is   free  software.   It   can  be  used,   modified  and
%% redistributed  by anybody for  personal or  commercial use.   The only
%% restriction  is  altering the  copyright  notice  associated with  the
%% material. Individuals or corporations are permitted to use, include or
%% modify the Erlguten engine.   All material developed with the Erlguten
%% language belongs to their respective copyright holder.
%% 
%%   Copyright Notice
%% 
%%   This  program is  free  software.  It  can  be redistributed  and/or
%% modified,  provided that this  copyright notice  is kept  intact. This
%% program is distributed in the hope that it will be useful, but without
%% any warranty; without even  the implied warranty of merchantability or
%% fitness for  a particular  purpose.  In no  event shall  the copyright
%% holder  be liable  for  any direct,  indirect,  incidental or  special
%% damages arising in any way out of the use of this software.
%%
%% Authors:   Joe Armstrong   <joe@sics.se>
%%            Mikael Karlsson <mikael.karlsson@creado.com>
%% Last Edit: 2003-03-03
%% =====================================================================

-module(erlguten_pdf_assemble).

-include("erlguten.hrl").

-compile(export_all).

-import(lists, [map/2, mapfoldl/3, member/2, reverse/1]).
-import(pdf, [f2s/1, i2s/1]).

make_pdf_file(OutFile, Info, Fonts, Pages, MediaBox) ->
    {Root, Ninfo, Os} = build_pdf(Info, Fonts, Pages, MediaBox),
    {ok, F} = file:open(OutFile, [binary,raw,write]),
    file:write(F, header()),
    {ok, Pos0} = file:position(F, cur),
    %% io:format("Os=~p~n", [Os]),
    {Posns, _} = mapfoldl(fun(I, Pos) -> 
				  {{obj,Index,_},_} = I,
				  Data = serialise(I),
				  file:write(F, Data),
				  {ok, Pos1} = file:position(F, cur),
				  {{Index,Pos}, Pos1}
			  end, Pos0, Os),
    %% io:format("Posn=~p~n",[Posns]),
    XrefStartPos = add_xref(F, Posns),
    add_trailer(F, Posns, Root, Ninfo),
    add_start_xref(F, XrefStartPos),
    file:close(F).

build_pdf(Info, Fonts, Pages, MediaBox) ->
    {Free,Fonts1,O1s}  = mk_fonts(Fonts, 1, [], []),
    %% io:format("here1:~p~n",[O1s]),
    PageTree = Free,
    {Free1,Ps,O3s} = mk_pages(Pages, PageTree, Free+1,[],[]),
    %% io:format("here2:~p~n",[O3s]),
    O2 = {{obj,PageTree,0}, mkPageTree(Ps, Fonts1, MediaBox)},
    Root = Free1,
    O4 = {{obj,Root,0}, mkCatalogue(PageTree)},
    %% io:format("Free1=~p~n",[Free1]),
    NInfo = Free1 + 1,
    O5 = {{obj,NInfo,0}, mkInfo(Info)},
    {Root, NInfo, O1s ++ [O2|O3s] ++ [O4,O5]}.
    
mk_fonts([], I, Fs, Os) -> 
    A = {{obj,I,0},{dict,map(fun({Alias, FontObj}) ->
		      {Alias, {ptr,FontObj,0}}
	      end, lists:reverse(Fs))}},
    {I+1, {ptr,I,0}, reverse([A|Os])};
mk_fonts([{FontName, Alias}|T], I, Fs, E) ->
    case builtinFont(FontName) of
	{yes, false} ->
	    O = {{obj,I,0},mkFont(FontName, Alias)},
	    mk_fonts(T, I+1, [{Alias,I}|Fs], [O|E]);
	{yes, true} ->
	    case erlguten_font_server:data(FontName) of
		{ok, Data} ->
		    O1 = {{obj,I,0}, mkFont1(FontName, Alias, I+1, Data)},
		    O2 = {{obj,I+1,0}, mkFontDescriptor(Data)},
		    mk_fonts(T, I+2, [{Alias,I}|Fs], [O2,O1|E]);
		error ->
		    io:format("You cannot use font:~s~n "
			      " substituting Times-Roman~n",
			      [FontName]),
		    O = {{obj,I,0},mkFont("Times-Roman", Alias)},
		    mk_fonts(T, I+1, [{Alias,I}|Fs], [O|E])
	    end;
	no ->
	    io:format("You cannot use font:~s~n substituting Times-Roman~n",
		      [FontName]),
	    O = {{obj,I,0},mkFont("Times-Roman", Alias)},
	    mk_fonts(T, I+1, [{Alias,I}|Fs], [O|E])
    end.
    
mk_pages([], _, N, P, O) -> {N, reverse(P), reverse(O)};
mk_pages([{page,Str}|T], Parent, I, L, E) ->
    O1 = {{obj,I,0},mkPageContents(Str)},
    O2 = {{obj,I+1,0},mkPage( Parent, I)},
    mk_pages(T, Parent, I+2, [I+1|L], [O2,O1|E]).

mkCatalogue(PageTree) ->
    {dict,[{"Type",{name,"Catalog"}},
	   {"Pages",{ptr,PageTree,0}}]}.

%% mkFont is used for the 14  inbuilt fonts
mkFont(FontName, Alias) ->
    {dict,[{"Type",{name,"Font"}},
	   {"Subtype",{name,"Type1"}},
	   {"Name",{name,Alias}},
	   {"BaseFont",{name,FontName}},
	   {"Encoding",{name,"MacRomanEncoding"}}]}.

mkFont1(FontName, Alias, FontDescriptorPrt, M) ->
    FirstChar = M#afm2.firstChar,
    LastChar = M#afm2.lastChar,
    Widths = M#afm2.widths,
    {dict,[{"Type",{name,"Font"}},
	   {"Subtype",{name,"Type1"}},
	   {"Name",{name,Alias}},
	   {"BaseFont",{name,FontName}},
	   {"Encoding",{name,"MacRomanEncoding"}},
	   {"FirstChar",FirstChar},
	   {"LastChar",LastChar},
	   {"Widths", {array,Widths}},
	   {"FontDescriptor",{ptr,FontDescriptorPrt,0}}]}.

mkFontDescriptor(M) ->
    #afm2{ascender=Ascent,capHeight=CapHeight,descender=Descent,
	  fontBBox=FontBBox1,baseFont=FontName,italicAngle=ItalicAngle,
	  stemV=StemV, xHeight=XHeight} = M,
    {X1,X2,X3,X4} = FontBBox1,
    FontBBox = [X1,X2,X3,X3],
    {dict,[{"Type",{name,"FontDescriptor"}},
	   {"Ascent",Ascent},
 	   {"CapHeight",CapHeight},
	   {"Descent",Descent},
	   {"Flags",98},
	   {"FontBBox",{array,FontBBox}},
	   {"FontName",{name,FontName}},
	   {"ItalicAngle",ItalicAngle},
	   {"StemV",StemV},
	   {"XHeight",XHeight}]}.

mkInfo(I) ->
    {dict,[{"Creator",{string,I#info.creator}},
	   {"CreationDate",{string,"D:" ++ I#info.creationDate}},
	   {"Producer",{string,I#info.producer}},
	   {"Author",{string,I#info.author}},
	   {"Title",{string,I#info.title}},
	   {"Subject",{string,I#info.subject}},
	   {"Keywords",{string,I#info.keywords}}]}.

%% L = [int()] = list of objects representing pages

mkPageTree(L, Fonts, MediaBox = {A,B,C,D} ) ->
    {dict,[{"Type",{name,"Pages"}},
	   {"Count",length(L)},
	   {"MediaBox", {array,[A,B,C,D]}},
	   {"Kids",{array,map(fun(I) ->{ptr,I,0} end,L)}},
	   {"Resources",
	    {dict,[{"Font", Fonts },
		   {"ProcSet", {array,[{name,"PDF"},{name,"Text"}]}}]}}]}.

%% Fonts = [{Name,PageNo}]
%%   example [{"F1",12},{"F7",15}]

mkPage(Parent, Contents) ->
    {dict, [{"Type", {name,"Page"}},
	    {"Parent", {ptr,Parent,0}},
	    {"Contents", {ptr, Contents, 0}}
	   ]}.

mkPageContents(Str) ->
    {stream, Str}.

serialise2bin(A) ->
    list_to_binary(serialise(A)).
serialise({stream, S}) ->
    B = list_to_binary(S),
    Len = size(B),
    ["<</Length ",i2s(Len),">>\nstream\n",B,"\nendstream\n"];
serialise({{obj,I,J},K}) ->
    [i2s(I)," ",i2s(J)," obj\n", serialise(K),"endobj\n"];
serialise({dict,L}) ->
    ["<<\n", map(fun({I,J}) ->
			 ["/",I," ",serialise(J),"\n"]
		 end, L),
     ">>\n"];
serialise({name, S}) ->
    [" /",S," "];
serialise({string, S}) ->
    [" (",S,") "];
serialise({ptr, I, J}) ->
    [" ",i2s(I)," ",i2s(J)," R "];
serialise({array, L}) ->
    [" [ ", map(fun(I) -> serialise(I) end, L), " ] "];
serialise(N) when integer(N) ->
    [" ",i2s(N), " "];
serialise(F) when float(F)->
      [" ",f2s(F), " "];
serialise(X) ->
    io:format("I cannot serialise:~p~n", [X]),
    exit(serialise).

 
header() ->
    "%PDF-1.3" ++ [8#015,$%,8#342,8#343,8#317,8#323, 8#015,8#012].

%% Objs = {ObjNo, Startpos}

add_xref(F, Objs) ->
    {ok, P} = file:position(F, cur),
    XrefStart = P,
    L  = ["xref\n0 ",i2s(length(Objs)+1),"\n",xref(0,"65535 f")|
	  map(fun({I,Pos}) -> xref(Pos,"00000 n") end, Objs)],
    file:write(F, L),
    XrefStart.

xref(I, Str) ->
    S = lists:flatten(io_lib:format("~10.10.0w", [I])),
    [S," ", Str,"\r\n"].


add_trailer(F, Objs, Root, Info) ->
    L = ["trailer << /Size ", i2s(length(Objs)+1),
	 " /Root ",i2s(Root), " 0 R ",
	 " /Info ",i2s(Info), " 0 R >>\n"],
    file:write(F, L).

add_start_xref(F, XrefStartPos) ->
    L = ["startxref\n",i2s(XrefStartPos),"\n%%EOF\n"],
    file:write(F, L).


%% xref
%% 0 9
%% 0000000000 65535 f 
%% 0000000033 00000 n 
%% 0000000098 00000 n 
%% 0000000144 00000 n 
%% 0000000203 00000 n 
%% 0000000231 00000 n 
%% 0000000409 00000 n 
%% 0000000721 00000 n 
%% 0000000835 00000 n 
%% trailer
%% <<
%% /Size 9
%% /Root 1 0 R
%% /Info 8 0 R
%% >>
%% startxref
%% 1073
%% %%EOF

pdfloop(PDFC, Stream)->
    receive
	{get_font_alias, Pid, Fontname} ->
	    {F,FA} = handle_setfont(PDFC#pdfContext.fonts, Fontname),
	    "F" ++ Str = FA,
	    Index = list_to_integer(Str),
	    erlguten_font_server:ensure_loaded(Fontname, Index),
	    Pid ! {self(), font_alias, Index},
	    pdfloop(PDFC#pdfContext{fonts=F}, Stream);
	{stream, {append, String}}->
	    pdfloop(PDFC, [Stream, String," "]);
	{font, {set, Fontname, Size}}->
	    {F,FA} = handle_setfont(PDFC#pdfContext.fonts, Fontname),
	    S = ["/", FA, " ",i2s(Size)," Tf "],
	    pdfloop(PDFC#pdfContext{fonts=F}, [Stream,S]);
	{image, FilePath}->
	    {I,IMG} = handle_image(PDFC#pdfContext.images, FilePath),
	    S = ["/", IMG, " Do\n"],
	    pdfloop(PDFC#pdfContext{images=I}, [Stream,S]);
	{page,{new, PID}}->
	    {Add, PageNo} = 
		handle_newpage(PDFC#pdfContext.pages,
			       PDFC#pdfContext.currentpage,
			       Stream),
	    PID ! {page, PageNo},
	    pdfloop(PDFC#pdfContext{pages=Add, currentpage=PageNo}, []);
    	{page,{set,PageNo}}->
	    {NewPages,NewStream} = handle_setpage(PDFC#pdfContext.pages,PageNo,
						  PDFC#pdfContext.currentpage, 
						  Stream),
	    
	    pdfloop(PDFC#pdfContext{pages=NewPages,currentpage=PageNo}, 
		    NewStream);		    
	{page,{get_no, PID}} ->
	    PID ! {page, PDFC#pdfContext.currentpage},
	    pdfloop(PDFC, Stream);	    
	{info,Info}->
	    NewInfo = handle_info(PDFC#pdfContext.info, Info),
	    pdfloop(PDFC#pdfContext{info=NewInfo}, Stream);
	{mediabox, Mediabox}->
	    pdfloop(PDFC#pdfContext{mediabox=Mediabox}, Stream);	    
	{export,PID} ->
	    %% add last page if necessary before exporting
	    PDF = case Stream of 
		[] ->		    
		    handle_export(PDFC);
		_ ->
		    {Add, PageNo} = handle_newpage(PDFC#pdfContext.pages,
						   PDFC#pdfContext.currentpage,
						   Stream),
		    handle_export(PDFC#pdfContext{pages=Add})
	    end,
	    PID ! {export, PDF},
	    pdfloop(PDFC, Stream);
	delete ->
	    done;
	_ ->
	    io:format("Not yet implemented"),
	    pdfloop(PDFC, Stream)
    end.

%% Internals

handle_setpage(Pages, PageNo, Current, Stream)->
    NewPageDict = orddict:store(Current,Stream,Pages),
    NewStream = orddict:fetch(PageNo, NewPageDict),
    {NewPageDict,NewStream}.

handle_newpage(Pages,0,Stream)->
    {Pages,1};
handle_newpage(Pages, Current, Stream )->
    NewPageDict = orddict:store(Current,Stream, Pages),
    {NewPageDict, orddict:size(NewPageDict)+1}.

handle_export(PDFC)->
    {Root, Ninfo, Os} = 
	build_pdf(PDFC#pdfContext.info, 
		  dict:to_list(PDFC#pdfContext.fonts),
		  lists:map(fun({Key,Val}) -> {page,Val} end,
			    orddict:to_list(PDFC#pdfContext.pages)),
		  PDFC#pdfContext.mediabox),
    Objs = lists:map(fun(I) -> serialise2bin(I) end , Os),
    erlguten_pdf_export:mkdoc(Objs, Root, Ninfo).

handle_setfont(FontDict, FontName)->
    %% If font already in Dictionary return Alias
    %% Else insert font with new unique Alias and return new Alias
    %% call the font server to ensure that the ets tables
    %% for the fonts get loaded
    case dict:find(FontName, FontDict) of
	{ok, Value} ->
	    {FontDict, Value};
	error ->
	    case builtinFont(FontName) of
		{yes,_} ->
		    Index = length(dict:to_list(FontDict)) + 1,
		    Alias = "F" ++ i2s(Index),
		    NewDict = dict:store(FontName,Alias,FontDict),
		    {NewDict, Alias};
		no ->
		    io:format("You cannot use font:~s, "
			      "substituting with Times-Roman~n",[FontName]),
		    handle_setfont(FontDict,"Times-Roman")
		end	    
    end.

handle_image(ImageDict, FilePath)->
    case dict:find(FilePath, ImageDict) of
	{ok, Value} ->
	    {ImageDict, Value};
	error ->
	    Alias = "Im" ++ 
		i2s(length(dict:to_list(ImageDict)) + 1),
	    NewDict = dict:store(FilePath,Alias,ImageDict),
	    {NewDict, Alias}
    end.

handle_info(I,{author,Author})->
    I#info{author=Author};
handle_info(I,{title,Title}) ->
    I#info{title=Title};
handle_info(I,{subject,Subject}) ->
    I#info{subject=Subject};
handle_info(I,{date,{Year,Month,Day}}) when Year < 100->
    handle_info(I,{date,{Year + 2000,Month,Day}});
handle_info(I,{date,{Year,Month,Day}})->
    MFill = if
		Month < 10 ->
		    "0";
		true ->
		    ""
	    end,
    DFill = if
		Month < 10 ->
		    "0";
		true ->
		    ""
	    end,
    I#info{creationDate=i2s(Year)++MFill++i2s(Month)++DFill++i2s(Day)++"1200"};
handle_info(I,{keywords,Keywords}) ->
    I#info{keywords=Keywords}.

%% builtinFont = {yes,EmbedWidths} | no

%% The first 14 fonts are completely built-in
builtinFont("Helvetica") -> {yes,false};
builtinFont("Helvetica-Bold") -> {yes,false};
builtinFont("Helvetica-Oblique") -> {yes,false};
builtinFont("Helvetica-BoldOblique") -> {yes,false};
builtinFont("Times-Roman") -> {yes,false};
builtinFont("Times-Bold") -> {yes,false};
builtinFont("Times-Italic") -> {yes,false};
builtinFont("Times-BoldItalic") -> {yes,false};
builtinFont("Courier") -> {yes,false};
builtinFont("Courier-Bold") -> {yes,false};
builtinFont("Courier-Oblique") -> {yes,false};
builtinFont("Courier-BoldOblique") -> {yes,false};
builtinFont("Symbol") -> {yes,false};
builtinFont("ZapfDingbats") -> {yes,false};
%% The next 25 fonts need width tables
builtinFont("AvantGarde-Book") -> {yes,true};
builtinFont("AvantGarde-BookOblique") -> {yes,true};
builtinFont("AvantGarde-Demi") -> {yes,true};
builtinFont("AvantGarde-DemiOblique") -> {yes,true};
builtinFont("Bookman-Demi") -> {yes,true};
builtinFont("Bookman-DemiItalic") -> {yes,true};
builtinFont("Bookman-Light") -> {yes,true};
builtinFont("Bookman-LightItalic") -> {yes,true};
builtinFont("Helvetica-Narrow") -> {yes,true};
builtinFont("Helvetica-Narrow-Oblique") -> {yes,true};
builtinFont("Helvetica-Narrow-Bold") -> {yes,true};
builtinFont("Helvetica-Narrow-BoldOblique") -> {yes,true};
builtinFont("NewCenturySchlbk-Roman") -> {yes,true};
builtinFont("NewCenturySchlbk-Italic") -> {yes,true};
builtinFont("NewCenturySchlbk-Bold") -> {yes,true};
builtinFont("NewCenturySchlbk-BoldItalic") -> {yes,true};
builtinFont("Palatino-Roman") -> {yes,true};
builtinFont("Palatino-Italic") -> {yes,true};
builtinFont("Palatino-Bold") -> {yes,true};
builtinFont("Palatino-BoldItalic") -> {yes,true};
builtinFont("ZapfChancery-MediumItalic") -> {yes,true};
builtinFont("Helvetica-Condensed") -> {yes,true};
builtinFont("Helvetica-Condensed-Bold") -> {yes,true};
builtinFont("Helvetica-Condensed-Oblique") -> {yes,true};
builtinFont("Helvetica-Condensed-BoldObl") -> {yes,true};
builtinFont(X) -> no.











