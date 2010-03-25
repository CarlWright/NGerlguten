-module(eg_lib).

-export([showGrid/2, moveAndShow/4]).
-import(pdf_op, [n2s/1]).

%% showGrid(PDF, a4 | usLetter) 
%%   adds a grid to the current page page

sizeOfPaper(a4) ->
    {595, 842};
sizeOfPaper(usLetter) ->
    {612, 792}.

showGrid(PDF, Paper) ->
    {PaperWidth, PaperHeight} = sizeOfPaper(Paper),
    %% Top = PaperHeight - 10,
    Top = 825, % hack
    Bottom = 10,
    Left = 10,
    %% Right = PaperWidth - 10,
    Right = 575,
    pdf:set_font(PDF,"Helvetica", 8),
    vlines(PDF, Left, Right, Top, Bottom),
    hlines(PDF, Left, Right, Top, Bottom).

hlines(PDF, Left, Right, Top, Bottom) ->
    diter(Top,25,10,
	  fun(Y) ->
		  %% pdf:set_fill_gray(PDF,1.0),
		  pdf:line(PDF, Left, Y, Left+20, Y),
		  pdf:line(PDF, Right, Y, Right-20, Y),
		  %% pdf:set_fill_gray(PDF,0.8),
		  pdf:line(PDF, Left+20,Y,Right-20,Y),
		  moveAndShow(PDF, Left, Y+2, n2s(Y)),
		  moveAndShow(PDF, Right-20, Y+2, n2s(Y)),
		  true
	  end).

vlines(PDF, Left, Right, Top, Bottom) ->
    diter(Right,25,10,
	  fun(X) ->
		  pdf:line(PDF, X, Top, X, Top-20),
		  moveAndShow(PDF, X-5, Top-35,n2s(X)),
		  pdf:line(PDF, X, Bottom, X, Bottom+20),
		  pdf:line(PDF, X, Top -40, X, Bottom + 35),
		  moveAndShow(PDF, X-5, Bottom+23,n2s(X))
	  end).

moveAndShow(PDF, X, Y, Str) ->
    pdf:begin_text(PDF),
    pdf:set_text_pos(PDF, X, Y),
    pdf:text(PDF, Str),
    pdf:end_text(PDF).

%% downwards iterator

diter(X, Inc, Stop, F) when X < Stop ->
    true;
diter(X, Inc, Stop, F) ->
    F(X), diter(X-Inc,Inc,Stop,F).
    
