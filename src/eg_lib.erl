%%==========================================================================
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

-module(eg_lib).

-export([find_files/3, find_files/5]).

-include_lib("kernel/include/file.hrl").



%%==========================================================================

%% Examples:
%%   find_files(".", "*.erl", false)
%%     finds all files in the current directory.
%%     Recursive scan of sub-directories is also allowed.
%%
%%   find_files(Dir, RegExp, Recursive, Fun/2, Acc0)
%%      applies Fun(File, Acc) -> Acc. to each file

find_files(Dir, Re, Flag) -> 
    Re1 = xmerl_regexp:sh_to_awk(Re),
    lists:reverse(find_files(Dir, Re1, Flag,
                             fun(File, Acc) ->[File|Acc] end, [])).

find_files(Dir, Reg, Recursive, Fun, Acc) ->
    case file:list_dir(Dir) of
	{ok, Files} -> 
	    find_files(Files, Dir, Reg, Recursive, Fun, Acc);
	{error, _}  -> 
	    Acc
    end.

find_files([File|T], Dir, Reg, Recursive, Fun, Acc0) ->
    FullName = Dir ++  [$/|File],
    case file_type(FullName) of
	regular ->
	    case re:run(FullName, Reg) of
		{match,_}  ->
		    Acc = Fun(FullName, Acc0),
		    find_files(T, Dir, Reg, Recursive, Fun, Acc);
		_ ->
		    find_files(T, Dir, Reg, Recursive, Fun, Acc0)
	    end;
	directory -> 
	    case Recursive of
		true ->
		    Acc1 = find_files(FullName, Reg, Recursive, Fun, Acc0),
		    find_files(T, Dir, Reg, Recursive, Fun, Acc1);
		false ->
		    find_files(T, Dir, Reg, Recursive, Fun, Acc0)
	    end;
	error -> 
	    find_files(T, Dir, Reg, Recursive, Fun, Acc0)
    end;
find_files([], _, _, _, _, A) ->
    A.

file_type(File) ->
    case file:read_file_info(File) of
	{ok, Facts} ->
	    case Facts#file_info.type of
		regular -> 
		    regular;
		directory -> 
		    directory;
		_ -> 
		    error
	    end;
	_ ->
	    error
    end.
