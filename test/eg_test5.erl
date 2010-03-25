-module(eg_test5).

-compile(export_all).
-import(eg_line_break, [break_richText/2]).
-import(eg_richText, [mk_test_word/1, str2richText/2]).
-import(eg_richText2pdf, [richText2pdf/8]).
-import(pdf_op, [i2s/1]).

test() ->
    test(9).

test(1) -> 
    text(1,11);
test(2) ->
    %% Noraml para test - fits exactly into three lines
    break_richText(text(1,11), {justified, [190,190,190]});
test(3) ->
    %% This has some splill
    break_richText(text(1,11), {justified, [190,190]});
test(4) -> 
    break_richText(text(1,11), {ragged, [190,190,190,190]});
test(5) -> 
    %% fits exactly
    break_richText(text(1,11), {preformatted, [200,200,200,200,200,200,200]});
test(6) -> 
    %% Has some spill
    break_richText(text(1,11), {preformatted, [200,200,200]});
test(7) -> 
    break_richText(text(1,11), {centered, [210,210,210,210,210,210,210,210]});
%% test the line breaker
test(8) ->
    W1 = mk_test_word("Have"),
    W2 = mk_test_word("a"),
    W3 = mk_test_word("splended"),
    W4 = mk_test_word("opportunity"),
    eg_line_break:make_partitions([W1,W2,W3,W4]);
test(9) ->
    PDF = pdf:new(),
    pdf:set_pagesize(PDF,a4),
    pdf:set_page(PDF,1),
    pdf:begin_text(PDF),
    pdf:set_font(PDF, "Times-Roman", 24),
    demo(PDF, 0, 350, 475, 12, 13, 0, 180),
    demo(PDF, 45, 325, 600, 11, 12, 0, 190),
    demo(PDF, 90, 250, 575, 11, 12, 0, 190),
    demo(PDF, 135, 150, 600, 10, 11, 0, 190),
    demo(PDF, 180, 300, 200, 14, 16, 0, 130),
    demo(PDF, 225, 150, 375, 11, 12, 0, 130),
    demo(PDF, 270, 125, 500, 9, 10, 0, 60),
    demo(PDF, 315, 375, 375, 12, 13, 10, 120),
    pdf:end_text(PDF),
    pdf_lib:showGrid(PDF, a4),
    Serialised = pdf:export(PDF),
    file:write_file("eg_test5.pdf",[Serialised]),
    pdf:delete(PDF).

demo(PDF, Rot, X, Y, PointSize, Leading, Offset, Width) ->
    Widths = [Width-Offset|lists:duplicate(30,Width)],
    Off = [Offset|lists:duplicate(30,0)],
    {Lines,_,_} = break_richText(text(1,Rot,PointSize), {justified, Widths}),
    Code = richText2pdf(X, Y, justified, Rot, Lines, Leading, Widths, Off),
    io:format("Code=~p~n",[Code]),
    pdf:append_stream(PDF, Code).

for(0, F) ->    
    true;
for(N, F) ->
    F(N),
    for(N-1, F).

justified_para(Leading, Offset, Width, N) ->
    justified_para(Leading, Offset, Width, 0, N).

justified_para(Leading, Offset, Width, Rot, N) ->
    {justified, Leading, Rot,
     [Offset|lists:duplicate(N-1, 0)],
     [Width-Offset|lists:duplicate(N-1,Width)]}.

ragged_para(Leading, Offset, Width, N) ->
    {ragged, Leading, 
     [Offset|lists:duplicate(N-1, 0)],
     [Width-Offset|lists:duplicate(N-1,Width)]}.

text(N, Pts) ->
    text(N, 0, Pts).

text(1, Rot, Pts) ->
    str2richText("Rotation =" ++ i2s(Rot) ++ " Hello joe how are you today?
May I take this opportunity
of saying
that my favorite color is blue.
Have a nice day,
from Mr. C. Computer.", Pts).











