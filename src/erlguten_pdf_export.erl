%%======================================================================
%% Purpose: Test PDF documents main api
%%----------------------------------------------------------------------
%% Copyright (C) 2003 Mikael Karlsson
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
%% Authors:   Mikael Karlsson <mikael.karlsson@creado.com>
%% Last Edit: 2003-03-11
%% =====================================================================

-module(erlguten_pdf_export).
%% Date:    2003-01-03
%% Purpose: Generate PDF file structure

-compile(export_all).
%% -export([batch/1, pack/3, unpack/1]).

%% Make a pdf document ready for export to file
mkdoc(Objects, RootRef, InfoRef) ->
    b([
       startmark(),
       pdfbmagic(),
       Objects,
       xref(Objects),
       trailer(Objects, RootRef, InfoRef),
       startxref(Objects),
       endmark()
      ]).

startmark() -> "%PDF-1.4\n".
endmark() ->   "%%EOF\r\n".
%% pdfbmagic() -> "zG_\\325\\371\\337J\\244\030\\267\\260#s6\\037\\246dR L\\204s\\037".
%% pdfbmagic() ->[8#015,$%,8#342,8#343,8#317,8#323, 8#015,8#012].
pdfbmagic()->
    [].
xref(Objects) ->
    ["xref\n",
     "0 ",i(nobjects(Objects) + 1), "\n",
     "0000000000 65535 f \n",
     xref(Objects, 
	  length(startmark()) + length(pdfbmagic())
	 )
    ].

xref([Obj|T], Accu) ->
    Accu1 = Accu + objsize(Obj),
    [i10(Accu) ++ " 00000 n \n" | xref(T, Accu1)];
xref([], Accu) ->
    [].

trailer(Objects, RootRef, InfoRef) ->
    ["trailer\n",
     "<<\n",
     "/Size ",i(nobjects(Objects) + 1), "\n",
     "/Root ",i(RootRef), " 0 R\n",
     "/Info ", i(InfoRef), " 0 R\n",
     ">>\n"].

startxref(Objects) ->
    ["startxref\n",
     i(lists:foldl(fun(A, Accu) -> objsize(A) + Accu end, 
		   length(startmark()), Objects)), 
     "\n"].


%% ----------------------------------------------------

nobjects(Objects) ->
    length(Objects).

%% The length of the indirect object
objsize(Obj) when binary(Obj)
		  -> size(Obj);
objsize(Obj) when list (Obj) ->
    size(b(Obj)).


%% -- A couple of useful functions I copied from
%% -- Joe Armstrongs erlpdf.erl, http://www.sics.se/~joe

b(X) -> list_to_binary(X).
i(X) -> integer_to_list(X).

i10(X) ->
    S = i(X),
    pad(10-length(S)) ++  S.

pad(N) when N > 0 -> [$0|pad(N-1)];
pad(_)            -> [].

%% -----------------
