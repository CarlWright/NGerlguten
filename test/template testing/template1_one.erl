-module (template1_one).

-include("../../include/eg.hrl").

-export ([on_instanciation/3, box/1, tag_map/1, handler/5]).

on_instanciation(Env, Page,PDF) ->
  ModName = atom_to_list(?MODULE),
  FilePart = string:substr( ModName, 1, (string:str( ModName, "_") - 1)),
  FileName = "../test/template testing/" ++ FilePart ++ ".tem",
  V = eg_xml_lite:parse_file(FileName),
   [{pi, _},{xml,{templates,[],[A]}}] = V,
   {template,[{"name","one"}],Boxes} = A,   % select the template that matches the 2nd half of the module name
    BoxRecords = boxParse(Boxes),           % tear off Box Definitions for a template
    Env1 = loadBoxRecords(Env,BoxRecords),
 %  {box,BoxList,Objects} = BoxDef, % repeat for each box definition in the template
  %  [{Name,Value} | MoreValues] = BoxList, % repeat for each value of a box record
  {Page, PDF}.
 
  
%% Parse out the descriptions of the boxes with their tags 

boxParse([BoxDef|MoreBoxes]) ->
  Record = #box{},
  {box,BoxList,Objects} = BoxDef,
  Record2 = setBoxFields(Record,BoxList),
  boxParse(MoreBoxes, [Record2]).

%% parse out the values for the Box record and return a box record
  
boxParse([],Records) ->
  Records;
boxParse([BoxFields|MoreFields],Records) ->
  Record = #box{},
  {box,BoxItem,Objects} = BoxFields,  
  Record2 = setBoxFields(Record,BoxItem),
  boxParse(MoreFields, Records ++ [Record2]). 

%% parse out each attribute & value for the box

setBoxFields(Record, []) ->
  Record;
setBoxFields(Record, [Field|MoreFields]) ->
  {Name,Value} = Field,
  Record2 = setBoxFields(Record,Name, Value),
  setBoxFields( Record2, MoreFields).
 
%% set the attributes of the box record
    
setBoxFields(Record,Name, Value ) ->
RecordOut = case Name of
  "bg" -> Record#box{bg = list_to_atom(Value)};
  "grid" -> Record#box{ grid = list_to_atom(Value)};
  "continue" -> Record#box{ continue = list_to_atom(Value)};
  "lines" -> Record#box{ lines = list_to_integer(Value)};
  "fontSize" -> Record#box{ fontSize = list_to_integer(Value)};
  "width" -> Record#box{ width = list_to_integer(Value)};
  "name" -> Record#box{ name = list_to_atom(Value)};
  "x" ->  Record#box{ x = list_to_integer(Value)};
  "y" ->  Record#box{ y = list_to_integer(Value)};  
  "free" ->  Record#box{ free = list_to_integer(Value)};  
  "leading" ->  Record#box{ leading = list_to_integer(Value)}
end.

loadBoxRecords(Env,BoxRecords) ->
  Dict = Env#env.dict,
  .



box(Box) ->
  
  Box.
  
tag_map(Box) ->
  
  Box.
  
handler(Box, ParaTag, Args, Data, Env) ->
  
  Env.
    