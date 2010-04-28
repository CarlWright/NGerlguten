-module (template1_one).

-include("../../include/eg.hrl").

-export ([on_instanciation/2, box/1, tag_map/1]).

on_instanciation(Page,PDF) ->
  ModName = atom_to_list(?MODULE),
  FilePart = string:substr( ModName, 1, (string:str( ModName, "_") - 1)),
  FileName = "../test/template testing/" ++ FilePart ++ ".tem",
  V = eg_xml_lite:parse_file(FileName),
  {Page, PDF}.
  
box(Box) ->
  
  Box.
  
tag_map(Box) ->
  
  Box.
  
handler(Box, ParaTag, Args, Data, Env) ->
  
  Env.
    