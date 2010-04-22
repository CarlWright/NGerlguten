-module (eg_table).



-export ([table/7]).

-record(doc_info, {system    = " ",
                   type      = " ",
                   reference = " ",
                   author    = " ",
                   version   = " ",
                   date      = " "}).

% State during main output routine
-record(st,
        {doc_info = #doc_info{},
         toc_num = [], % toc after page numbering
         toc = [],     % toc before page numbering
         y = 735,      % How far up the current page; starting position is 700
         min_y = 60,   % Bottom margin - bottom edge of page to last text line
         max_y = 735,  % Top margin - bottom edge of page to top of text area
         page = 1,     % Current Page Number
         fig = 1,      % Current figure number
         pending_images = []
        }).
           
        
%% Creating tables
%%
%% 1. work out number of columns
%% 2. split space evenly, leaving an extra 4 Pts for white space around each,
%%    and an extra 1 Pt for each vertical Line
%% 2 bis. OR Turn each cell into RTF as if it had the full width to use.

%%           Then find a bunch of metrics about the table which will
%%           help us find a good set of column widths.

%%           Find the longest word in each column. This sets the
%%           minimum column width. 

%%           If the sum of the longest words is greater than the total
%%           width avaialable then issue a warning, but carry on with
%%           the table extending off the page to the right (really
%%           trick would be to switch to landscape mode automatically
%%           :)

%%           Ideally we want to fit into the minimum vertical space,
%%           so if there is a solution where every or most rows can
%%           fit on one line this would be very nice.

%%            Could do proportional split based on total volume of text
%%            in each column
%%
%%           We could find the longest cell for each column

%%   If all cells in a column will fit on a single line within what
%%   would be an even split between the columns, allow that column to
%%   take up the space it needs to fit everything one one line? OK,
%%   but at the limit might really penalise another column with lots
%%   of text.

%% 3. Convert the text into RTF lines for each cell
%% 4. For each row work out the number of lines required 
%%    (largest number of lines).
%% 5. Output the table, drwing lines as we go
%%
%% TODO - Parameterise by: Font size, Spacing around text, table width
%%        Clever algorithm to arrange column widths
%%        Do page breaks in the middle of a table
%% @spec table(PDF, Rows, X, Y, SO) ->
%%          Total_Y


table(PDF, Rows, X, Width, Start, Bottom, FontSize) ->
    S0 = #st{y=Start, min_y = Bottom },
    S = space_before(10, S0),
    Cols = max_length(Rows,0),      % Number of cols is max cols of all rows.
    Col_width = Width div Cols,
    W = Width - 5*Cols,

    RTF_words = lists:map(fun(Row) -> 
                                row2rtf(Row, lists:duplicate(Cols, W),FontSize) 
                        end, Rows),

    %% Find the longest single word in each column. Start with a minimum col
    %% size of 20000 milliPts. This gives reasonable minimum column widths
    Longest_words = tuple_to_list(
                      longest_words(RTF_words, erlang:make_tuple(Cols, 28000))),
    io:format("Longest word = ~p~n",[Longest_words]),
    Min_tab_width = lists:sum(Longest_words) + Cols * 5000,
    if Min_tab_width div 1000 + Cols > W -> % round up each col to next Pt
            io:format("*** Warning *** "
                      "Table will not fit into available width ~p~n", 
                      [Min_tab_width div Cols + Cols]);
       true -> ok
    end,
    _Text_width = Col_width - 4 - 1, % leave space for lines + ws

    %% Sum the total length of text in each column. This gives a
    %% general measure of the size of a column.
    Volumes = word_volumes(RTF_words, erlang:make_tuple(Cols, 0)),
    io:format("Volumes = ~p~n",[Volumes]),

    %% Find the longest single cell in each column. This does two
    %% things - it allows us to contract the whole table width if all
    %% columns fit completely within the overall width on a single
    %% line. It also might allow us to squeeze things around so that
    %% some columns are all one liners...

    Max_cell_widths = longest_cell(Volumes, erlang:make_tuple(Cols, 0)),
    io:format("Max Cell Widths = ~p~n",[Max_cell_widths]),

    Col_volume = sum_cells(Volumes, lists:duplicate(Cols, 0)),
    io:format("Column volumes = ~p~n",[Col_volume]),

    %% Try to share the column widths out respecting the minimums,
    %% but in proportion to the Volumes

    %% 1. Distribute by volume.
    I1 = lists:map(fun(Col) ->
                           Col / lists:sum(Col_volume) * W * 1000
                   end, Col_volume),
    io:format("I1 = ~p~n",[I1]),

    %% 2. Increase any columns which have been given less than their minimum
    {I2, Lost} = ensure_minimums(I1, Longest_words, 0, []),
    io:format("I2 = ~p~n",[{Lost, I2}]),

    %% 3. Try to give out any reductions amongst the other columns,
    %% not allowing them to go below minimum.
    I3 = distribute_reduction(I2, Longest_words, Lost),
    io:format("I3 = ~p~n",[I3]),


    %% 4. Reduce any cols which have been given more space than they
    %% need, noting which ones they are.

    %% 5. Try to give out any extra amongst cols which need it in
    %% proportion to their volume, not increasing the table width
    %% beyond the max width. Go back to step 4 to make sure we have
    %% not given any column too much. This recursion is handled by
    %% expand/3.

    I5bis = expand(I3, Max_cell_widths, W*1000),

    %% 6. Normalise, rounding up to the nearest Pt.
    I6 = lists:map(fun({fixed, Val}) ->
                           round(Val / 1000) + 1;
                      (Val) ->
                           round(Val / 1000) + 1
                   end, I5bis),
    io:format("I6 = ~w~n",[I6]),

    RTFRows = lists:map(fun(Row) ->
                                %% io:format("RTFRow = ~p~n",[Row]),
                                row2rtf(Row, I6, FontSize)
                        end, Rows),
    % io:format("RTFRows = ~p~n",[RTFRows]),
    Heights = lists:map(fun(Row) -> max_row_lines(Row,0) end, RTFRows),
    % io:format("Heights = ~p~n",[Heights]),

    %% Re-add the 2 Pt gap and 1 Pt line width for each column
    I7 = lists:map(fun(Text_w) -> Text_w + 5 end, I6),

    %% Don't start a table near the bottom of a page so we don't get
    %% only a top line, and because it don't look good to start it at
    %% the bottom.
    case S#st.y - 26 < S#st.min_y of
      true -> not_fitting;
      false -> rows(PDF, Heights, RTFRows, X, I7, Cols, false, S, FontSize)
    end.
              

    
    
     
    
%% Insert some vertical space unless we are at the top of the page
space_before(Pts, S) ->
    if S#st.y == S#st.max_y ->
            S;
       true ->
            S#st{y = S#st.y - Pts}
    end.



max_length([{_Tag, _, Row} | Rows], Max) ->
    if  length(Row) > Max -> max_length(Rows, length(Row));
        true              -> max_length(Rows, Max)
    end;
max_length([], Max) ->
    Max.
    
    
row2rtf({row, _, Row}, Col_widths, FontSize) ->
    TagMap = {[cell], [{default, eg_richText:mk_face("Times-Roman",FontSize,
                                                     true,default,0)},
                       {em,      eg_richText:mk_face("Times-Italic",FontSize,
                                                     true,default,0)},
                       {code,    eg_richText:mk_face("Courier",FontSize,
                                                     true,default,0)},
                       {b,       eg_richText:mk_face("Times-Bold",FontSize,
                                                     true,default,0)}]},
    row2rtf1(Row, Col_widths, TagMap);
row2rtf({header, _, Row}, Col_widths,FontSize) ->
    TagMap = {[cell], [{default, eg_richText:mk_face("Times-Bold",FontSize,
                                                     true,default,0)},
                       {em,      eg_richText:mk_face("Times-Bold",FontSize,
                                                     true,default,0)},
                       {code,    eg_richText:mk_face("Courier-Bold",FontSize,
                                                     true,default,0)},
                       {b,       eg_richText:mk_face("Times-Bold",FontSize,
                                                     true,default,0)}]},
    row2rtf1(Row, Col_widths, TagMap).


row2rtf1([Cell|T], [Col_width|T1], TagMap) ->
    %%io:format("Cell = ~p~n",[{Col_width, Cell}]),
    Norm = eg_xml2richText:normalise_xml(Cell, TagMap),
    {cell, _, RichText} = Norm,
    %%io:format("Norm = ~p~n",[RichText]),
    {Lines, _,_} =
        eg_line_break:break_richText(RichText, {justified, [Col_width]}),
    %% io:format("Lines = ~p~n",[Lines]),
    [{Lines, [Col_width], [0]}|row2rtf1(T, T1, TagMap)];
row2rtf1(_, [Cw|T], TagMap) ->
    [{[], [Cw], [0]}|row2rtf1([], T, TagMap)];
row2rtf1([], [], _) ->
    [].


%% Find the longest word in each column
longest_words([Row|T], Array) ->
    A1 = lws(Row, 1, Array),
    longest_words(T, A1);
longest_words([], A) ->
    A.
    
    
%% Sum the total lengths of all text per cell
word_volumes(RTF, Arr) ->
    lists:foldl(fun(Row, A1) ->
                        {A2, _} = row_volumes(Row, Arr),
                        A1 ++ [tuple_to_list(A2)]
                end, [], RTF).


%% Given the output of word_volumes/2, find the max of each column.
longest_cell(Arr_list, Arr) ->
    lists:foldl(fun(Row, A) ->
                        max_vals(Row, A)
                end, tuple_to_list(Arr), Arr_list).


sum_cells(Arr_list, Arr) ->    
    lists:foldl(fun(Row, A) ->
                        sum_vals(Row, A)
                end, Arr, Arr_list).

ensure_minimums([H|T], [H1|T1], Lost, Res) ->
    if H > H1 ->
            ensure_minimums(T, T1, Lost, Res ++ [H]);
       true ->
            ensure_minimums(T, T1, Lost + H1 - H, Res ++ [{fixed, H1}])
    end;
ensure_minimums([], [], Lost, Res) ->
    {Res, Lost}.


distribute_reduction(Cols, Minimums, Lost) ->
    Sum = lists:foldl(fun({fixed, _Col}, Sum) ->
                              Sum;
                         (Col, Sum) ->
                              Sum + Col
                      end, 0, Cols),
    distribute_reduction(Cols, Minimums, Sum, Lost).

distribute_reduction([{fixed, Col}|T], [_Min|T1], Sum, Lost) ->
    [{fixed, Col}|distribute_reduction(T, T1, Sum, Lost)];
distribute_reduction([Col|T], [Min|T1], Sum, Lost) ->
    Reduced = Col - Col/Sum*Lost,
    if Reduced < Min -> %% We can't take enough away. Darn!
            New_lost = Lost - (Min - Reduced),
            New_lost1 = if New_lost < 0 ->
                                0;
                           true ->
                                New_lost
                        end,
            [{fixed, Min}|distribute_reduction(T, T1, Sum, New_lost1)];
       true ->
            [Reduced|distribute_reduction(T, T1, Sum, Lost)]
    end;
distribute_reduction([], [], _Sum, _Lost) ->
    [].

%% Recursively go through giving out any saved in earlier steps and
%% then ensuring that we have not given out too much, until all
%% columns are fixed, or

expand(Init, Max_cell_widths, W) ->
    io:format("Init = ~p~n",[{Init, Max_cell_widths, W}]),
    {I1, Gained} = ensure_maximums(Init, Max_cell_widths, 0, []),
    io:format("I1 Gained = ~p~n",[{Gained, I1}]),
    Sum = lists:foldl(fun({fixed, Col}, Sum) ->
                              Sum + Col;
                         (Col, Sum) ->
                              Sum + Col
                      end, 0, I1),
    io:format("Sum = ~p~n",[Sum]),
    Fixed = is_fixed(Init),
    io:format("Fixed = ~p~n",[Sum]),
    io:format("W = ~p~n",[W]),
    if (Sum >= W - 1) or Fixed ->
            I1;
       true ->
            I2 = distribute_increase(I1, Sum, W),
            io:format("I2 Increase = ~p~n",[I2]),
            expand(I2, Max_cell_widths, W)
    end.



max_row_lines([{Row,_,_} | Rows], Max) ->
    if  length(Row) > Max -> max_row_lines(Rows, length(Row));
        true              -> max_row_lines(Rows, Max)
    end;
max_row_lines([], Max) ->
    Max.


rows(PDF, [H|T],[Row|Rows],X,Col_widths, Cols, Mid_row, S,FontSize) ->
    %% TODO - Work out when to draw top line - i.e. when we are really
    %% at the start of a row even in a multipage row
    if Mid_row == false ->
            eg_pdf:rectangle(PDF, X,S#st.y,lists:sum(Col_widths) + 1,1, fill);
       true -> ok
    end,
    if S#st.y - (H*FontSize) =< S#st.min_y ->
            This_page_lines = (S#st.y - S#st.min_y) div FontSize,
            {TPR, NPR} = split_row(This_page_lines, Row),
            S1 = row(PDF, TPR, X, Col_widths, This_page_lines, Cols, S, FontSize),
            %% io:format("NPR = ~p~n", [NPR]),
            %% Draw a line at the bottom as 
            if NPR == [] ->
                    eg_pdf:rectangle(PDF, X,S1#st.y,lists:sum(Col_widths) + 1,1,
                                     fill);
               true -> ok
            end,
            Mid_row_2 = NPR /= [],
            %% We need a new page, but don't want pending images
            %% appearing in the middle of the table hence 'false'.
            %% S2 = new_page(PDF, false, S1, FontSize),
            rows(PDF, [H - This_page_lines|T], 
                         [NPR|Rows], X, Col_widths, Cols, Mid_row_2, S1, FontSize);
       true ->
            Y = S#st.y,
            S1 = row(PDF, Row, X,Col_widths,H, Cols, S, FontSize),
            rows(PDF, T, Rows, X, Col_widths, Cols, false,
                 S1#st{y = Y - (H*FontSize)-5},FontSize)
    end;
rows(PDF, [], [], X, Col_widths, _Cols, _Mid_row, S, _) ->
    %% Draw final line under table
    eg_pdf:rectangle(PDF, X,S#st.y,lists:sum(Col_widths) + 1,1, fill),
    S.



lws([{Lines, _, _}|T], N, A) ->
    Lw = lw(Lines, 0),
    if (element(N, A) < Lw) ->
            lws(T, N+1, setelement(N, A, Lw));
       true ->
            lws(T, N+1, A)
    end;
lws([], _, A) ->
    A.


row_volumes(Row, Arr) ->
    %% io:format("Vols ~p~n", [Arr]),
    lists:foldl(fun({L, _, _}, {A1, Col_num}) ->
                        Vol = line_volume(L),
                        Old = element(Col_num, A1),
                        New = setelement(Col_num, A1, Old + Vol),
                        {New, Col_num + 1}
                end, {Arr, 1}, Row).


words_volume(Words, Vol) ->
    %% io:format("Word ~p~n", [{Vol, Words}]),
    lists:foldl(fun({word, L, _, _}, V) ->
                        V + L;
                   ({space, L, _}, V) ->
                        V + L
                end, Vol, Words).


line_volume(Line) ->
    %% io:format("Line~n"),
    lists:foldl(fun({richText, Words}, V) ->
                        words_volume(Words, V)
                end, 0, Line).

max_vals([H|T], [H1|T1]) ->
    if H > H1 ->
            [H|max_vals(T, T1)];
       true ->
            [H1|max_vals(T, T1)]
    end;
max_vals([], []) ->
    [].

sum_vals([H|T], [H1|T1]) ->
    [H+H1|sum_vals(T, T1)];
sum_vals([], []) ->
    [].
            

ensure_maximums([{fixed, H}|T], [_Max_needed|T1], Gain, Res) ->
    ensure_maximums(T, T1, Gain, Res ++ [{fixed, H}]);
ensure_maximums([H|T], [Max_needed|T1], Gain, Res) ->
    io:format("ensure_maximums - ~p~n",[{Gain , H , Max_needed}]),
    if Max_needed =< H ->
            ensure_maximums(T, T1, Gain + H - Max_needed,
                            Res ++ [{fixed, Max_needed}]);
       true ->
            ensure_maximums(T, T1, Gain, Res ++ [H])
    end;
ensure_maximums([], [], Gain, Res) ->
    {Res, Gain}.


distribute_increase(Cols, Sum, W) ->
    Gain = W - Sum,
    To_share = lists:foldl(fun ({fixed, _Col}, Sum1) -> Sum1;
                               (Col, Sum1)           -> Sum1 + Col
                           end, 0, Cols),
    distribute_increase1(Cols, Gain, To_share).
    
distribute_increase1([{fixed, Col}|T], Gain, To_share) ->
    [{fixed, Col}|distribute_increase1(T, Gain, To_share)];
distribute_increase1([Col|T], Gain, To_share) ->
    Inc = Col/To_share * Gain,
    io:format("Inc = ~p~n",[{Col, To_share, Gain}]),
    [Col + Inc|distribute_increase1(T, Gain, To_share)];
distribute_increase1([],_,_) ->
    [].

is_fixed([{fixed, _}|T]) ->
    is_fixed(T);
is_fixed([_|_]) ->
    false;
is_fixed([]) ->
    true.


row(PDF, [{Lines, Width, Off}|Cells], X, [Col_width|T], Height, Cols, S, FontSize) ->
    eg_pdf:rectangle(PDF, X,S#st.y,1,-(Height*FontSize)-5, fill),
    eg_pdf:begin_text(PDF),
    lines2pdf(PDF, X+3,S#st.y,Lines, FontSize, Width, Off, justified),
    eg_pdf:end_text(PDF),
    row(PDF, Cells, X+Col_width, T, Height, Cols-1, S, FontSize);
row(PDF, [], X, [], Height, 0, S, FontSize) ->
    eg_pdf:rectangle(PDF, X,S#st.y,1,-(Height*FontSize)-5, fill),
    S;
row(PDF, [], X, [Col_width|T], Height, Cols, S, FontSize) ->
    eg_pdf:rectangle(PDF, X,S#st.y,1,-(Height*FontSize)-5, fill),
    row(PDF, [], X+Col_width, T, Height, Cols-1, S, FontSize).



%% Split a table row into two rows where the first has no more
%% than Height number of lines in any cell, and the second row
%% contains the remaining lines.
%% TPR - This Page Rows
%% NPR - Next Page Rows
%% TPL - this page lines etc
split_row(Height, Cells) ->
    lists:foldl(fun({Cell, W, O}, {TPR, NPR}) ->
                        {TPL, NPL} = if length(Cell) =< Height ->
                                             {Cell, []};
                                        true ->
                                             lists:split(Height, Cell)
                                     end,
                        {TPR ++ [{TPL, W, O}], NPR ++ [{NPL, W, O}]}
                end, {[], []}, Cells).


lines2pdf(PDF, X,Y,Lines, Leading, Widths, Off, Justification) ->
    %% io:format("Input = ~p~n",[{X, Y, Justification, 0, Lines, 
%%                               Leading, Widths, Off}]),
    Code = eg_richText2pdf:richText2pdf(X, Y, Justification, 0, Lines, 
                                        Leading, Widths, Off),
    %io:format("Code = ~p~n",[Code]),
    eg_pdf:append_stream(PDF, Code).
    

lw([{richText, Words}|T], L) ->
    L1 = lw1(Words, L),
    lw(T, L1);
lw([], L) ->
    L.

lw1([{word, L, _, _}|T], L0) ->
    if(L > L0) ->
            lw1(T, L);
      true ->
            lw1(T, L0)
    end;
lw1([_|T], L) ->
    lw1(T, L);
lw1([], L) ->
    L.
  
