%%======================================================================
%% eg_pdf_page.erl PDF Pages
%%----------------------------------------------------------------------
%% Copyright (C) 2004 Mikael Karlsson
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
%% Last Edit: 2004-06-21
%% =====================================================================


%% @doc Page.
%%
%% <p>Purpose: Create PDF Pages </p>
%% <p>       Ref. Chapter 3.6.2 Page objects in PDF reference v1.4 </p>
%% 
%% <p>
%% <dl>
%%  <dt>/Type</dt>    <dd>/Page</dd>
%%  <dt>/Parent</dt> <dd>dictionary, Required; indirect reference. The parent
%%      page tree node.</dd>
%%  <dt>/LastModified</dt>
%%    <dd>date, Required if PieceInfo</dd>
%%  <dt>/Resources</dt>       
%%    <dd>dictionary. Required, inheritable. The resources required by the 
%%        page.</dd>
%%  <dt>/MediaBox</dt>    <dd>rectangle. Required, inheritable. Boundaries 
%%        of physical medium.</dd>
%%  <dt>/CropBox</dt>     <dd>rectangle. Optional, inheritable. Visible region
%%        of default user space. Defaults to value of MediaBox</dd>
%%  <dt>/BleedBox</dt>       
%%    <dd>rectangle, Optional. Clipping region for production environment.
%%        Defaults to value of CropBox.</dd>
%%  <dt>/TrimBox</dt>       
%%    <dd>rectangle, Optional. Intended dimensions of finished page after
%%        trimming.</dd>
%%  <dt>/ArtBox</dt><dd>rectangle, Optional. 
%%       Intended dimensions of the finished page after trimming.</dd>
%%  <dt>/BoxColorInfo</dt>  
%%    <dd>dictionary, Optional. Se PDF reference</dd>
%%  <dt>/Contents</dt>      <dd>stream or array. Content stream.</dd>
%%  <dt>/Rotate</dt>      
%%     <dd>integer, Optional, inheritable. The number of degrees the page
%%         is rotated cloclwise when displayed.</dd>
%%  <dt>/Group</dt>       
%%    <dd>dictionary, Optional. See PDF reference</dd>
%%  <dt>/Thumb</dt> <dd>stream, Optional. Stream object defining the page's
%%     thumbnail image.</dd>
%%  <dt>/B</dt>     <dd>array, Optional. Array of indirect references to 
%%       article beads appearing on the page.</dd>
%%  <dt>/Dur</dt>   <dd>number, Optional. Display duration in number of 
%%      seconds during presentations.</dd>
%%  <dt>/Trans</dt>     <dd>dictionary, Optional. Transition dictionary
%%       for presentations</dd>
%%  <dt>/Annots</dt>      <dd>array, Optional. Array of annotation 
%%   dictionaries.</dd>
%%  <dt>/AA</dt> <dd>dictionary, Optional. Additional actions dictionary</dd>
%%  <dt>/Metadata</dt> <dd>stream, Optional. Metadata for the page</dd>
%%  <dt>/PieceInfo</dt> <dd>dictionary, Optional. See PDF reference.</dd>
%%  <dt>/StructParents</dt> <dd>integer, Required if the page contains
%%      structural content items.</dd>
%%  <dt>/ID</dt> <dd>string, Optional. Digital id of the page parent Web 
%%     Capture content set.</dd>
%%  <dt>/PZ</dt> <dd>number, Optional. Preferred zoom factor.</dd>
%%  <dt>/SeparationInfo</dt> <dd>dictionary, Optional. Separation dictionary
%%     to generate color separations fopr the page.</dd>
%% </dl>
%% </p>
%% @end

-module(eg_pdf_page).

-import(lists, [map/2, mapfoldl/3, member/2, reverse/1]).
-import(eg_pdf_lib,[find_in_dict/2, store_in_dict/2, 
		    get_objects_of_type/2, add_object/2, make_object/2, 
		    search_object/2, store_object/2, get_ref/1, 
		    pdf_item/1]).

%% -compile(export_all).
-export([page/3,append_page/2, append_page2tree/2, append_to_page/3, 
	get_page/2, get_page_contents/2, page_tree/5]).

%% @spec page(ParenRef::integer(), ContentRef::integer(), Options) -> dict()
%% Options = [Option]
%% Option = {'"LastModified"', date()} | {'"Resources"', dict() } | 
%% {'"Annots"', array() } | {Key, pdftype()}
page(ParentRef, ContentsRef, Options) ->
    {dict, [{"Type", {name,"Page"}},
	    {"Parent", {ptr,ParentRef,0}},
	    {"Contents", {ptr, ContentsRef, 0}}
	    | lists:map( fun page_opt/1, Options ) ]}.

page_opt({"LastModified" = A, {date, Date}}) ->
    {A,{date, Date}};
page_opt({"Resources", {dict,_}} = A) ->
    A;
page_opt({"Resources", {ptr,_,_}} = A) ->
    A;
page_opt({"Annots", _}=A ) ->
    A;
page_opt({A,B}) ->
    {A,B}.

%% @spec page_tree(KidRefs, FontsPtr::ptr(), XObjectsPtr::ptr(), MediaBox::rect(), ProcSet) -> dict()
%% KidRefs = [ number() ]
%% ProcSet = imageb | imagec | imagebc
%% @doc Creates a Pages (Page Tree) dictionary
page_tree(KidRefs, FontsPtr, XObjectsPtr, MediaBox = {rect,{A,B,C,D}}, ProcSet ) ->
    ImProcSet = case ProcSet of
		    imagebc -> [{name, "ImageB"},{name, "ImageC"}];
		    imageb -> [{name, "ImageB"}];
		    imagec -> [{name, "ImageC"}];
		    _ -> []
		end,
    {dict,[{"Type",{name,"Pages"}},
	   {"Count",length(KidRefs)},
	   {"MediaBox", MediaBox },
	   {"Kids",{array,map(fun(I) ->{ptr,I,0} end,KidRefs)}},
	   {"Resources",
	    {dict,[{"Font", FontsPtr },{"XObject", XObjectsPtr },
		   {"ProcSet", {array,[{name,"PDF"},{name,"Text"}|ImProcSet]}}]}}]}.


append_page(PageContents, Objects) ->
    [PageTreeObj] = get_objects_of_type("Pages", Objects),
    PTRef = get_ref(PageTreeObj),
    {ContentRef,Objects1} = add_object({stream, PageContents}, Objects),
    {PageRef, Objects2} = add_object(page(PTRef,ContentRef,[]), Objects1),
    NewPTreeItem = append_page2tree(PageRef, pdf_item(PageTreeObj)),
    Objects3 = store_object(make_object(PTRef, NewPTreeItem),  Objects2),
    Objects3.


append_page2tree(Ref, PageTreeItem) ->
    Key = "Kids",
    {array, Kids} = find_in_dict(Key, PageTreeItem),
    NewKids = Kids ++ [{ptr, Ref, 0}], 
    store_in_dict({Key, {array, NewKids}}, PageTreeItem).

get_page(PageNo, Objects) ->
    [PageTreeObj] = get_objects_of_type("Pages", Objects),
    PageTree = pdf_item(PageTreeObj), 
    {array, Kids} = find_in_dict("Kids", PageTree),
    PagePtr = lists:nth(PageNo, Kids),
    case search_object(PagePtr, Objects) of
	{value, Object} ->
	    Object;
	false ->
	    no_page_available
    end.

get_page_contents(PageObj, Objects) ->
    PageDict = pdf_item(PageObj),
    case find_in_dict("Contents", PageDict) of
	{ptr, I, J} = Ptr ->
	    {value, Object} = search_object(Ptr, Objects),
	    Object;
	PtrList ->
	    lists:map(fun(Ptr) ->
		      {value, Object} = search_object(Ptr, Objects),
		      Object 
		      end, PtrList)
    end.

append_to_page(S, PageObj, Objects) ->    
    {Key, {stream, Str}} = get_page_contents(PageObj, Objects),
    store_object({Key, {stream, Str ++ S}}, Objects).

%% --- Page sizes ---
pagesize(a0)             -> pagesize( 2380, 3368 );
pagesize(a1)             -> pagesize( 1684, 2380 );
pagesize(a2)             -> pagesize( 1190, 1684 );
pagesize(a3)             -> pagesize( 842, 1190 );
pagesize(a4)             -> pagesize( 595, 842 );
pagesize(a5)             -> pagesize( 421, 595 );
pagesize(a6)             -> pagesize( 297, 421 );
pagesize(a7)             -> pagesize( 210, 297 );
pagesize(a8)             -> pagesize( 148, 210 );
pagesize(a9)             -> pagesize( 105, 148 );
pagesize(b0)             -> pagesize( 2836, 4008 );
pagesize(b1)             -> pagesize( 2004, 2836 );
pagesize(b2)             -> pagesize( 1418, 2004 );
pagesize(b3)             -> pagesize( 1002, 1418 );
pagesize(b4)             -> pagesize( 709, 1002 );
pagesize(b5)             -> pagesize( 501, 709 );
pagesize(b6)             -> pagesize( 355, 501 );
pagesize(b7)             -> pagesize( 250, 355 );
pagesize(b8)             -> pagesize( 178, 250 );
pagesize(b9)             -> pagesize( 125, 178 );
pagesize(b10)            -> pagesize( 89, 125 );
pagesize(c5e)            -> pagesize( 462, 649 );
pagesize(comm10e)        -> pagesize( 298, 683 );
pagesize(dle)            -> pagesize( 312, 624 );
pagesize(executive)      -> pagesize( 542, 720 );
pagesize(folio)          -> pagesize( 595, 935 );
pagesize(ledger)         -> pagesize( 1224, 792 );
pagesize(legal)          -> pagesize( 612, 1008 );
pagesize(letter)         -> pagesize( 612, 792 );
pagesize(tabloid)        -> pagesize( 792, 1224 ).

pagesize(Width, Height) -> {0,0,Width,Height}.

set_pagesize(Size)-> 
    {rect, pagesize(Size) }.

set_pagesize(Width, Height) -> 
    {rect, pagesize(Width, Height)} .

