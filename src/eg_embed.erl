%%======================================================================
%% Purpose: Font embedding code
%%----------------------------------------------------------------------
%% Copyright (C) 2003 Joe Armstrong
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
%% Authors:   Joe Armstrong <joe@sics.se>
%% Last Edit: 2003-03-12
%% =====================================================================


-module(eg_embed).

-compile(export_all).
-import(lists, [map/2]).

%% The format of a PBF is specified in 5040.Download_Fonts.pdf
%% It is a sequence of chunks
%%   <<128, Type, N1, N2, N3, N4 >> <<Len bytes>>
%%   <<128, 3>>

%% Where
%%    Type = 1 -> ascii
%%    Type = 2 -> binary
%%    Len = N1 + N2*256 + N3*256*256 + N4*256*256*256,

test() ->
    embed("brush.pfb").

embed(F) ->
    P = parse_pfb(F),
    O = map(fun({_,_,B}) -> B end, P),
    file:write_file(F ++ ".synth", O).

%% Parse_pfb -> [{Type,Len,Bin}]
%%  The lengths are the required lengths in the
%%  object descriptor ...

parse_pfb(F) ->
    {ok, Bin} = file:read_file(F),
    L = parse(Bin).

parse(<<128,3>>) ->
    [];
parse(B) ->
    {B1,B2} = split_binary(B, 6),
    [128,Type,N1,N2,N3,N4] = binary_to_list(B1),
    Len = N1 + N2*256 + N3*256*256 + N4*256*256*256,
    %% io:format("Chunk: ~p length=~p~n",[Type, Len]),
    case Len of
	0 -> [];
	_ ->
	    {B3,B4} = split_binary(B2, Len),
	    [{Type,Len,B3}|parse(B4)]
    end.




