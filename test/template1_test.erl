-module (template1_test).
-include("eg.hrl").
-include_lib("eunit/include/eunit.hrl").

run_test() ->
  ?debugMsg("Begin Test"),
  ok.

on_instanciation(Env ) ->
  FilePart = atom_to_list(?MODULE),
  FileName = "../test/template testing/" ++ FilePart ++ ".tem",
  Env2 = Env#env{template = ?MODULE},
  V = eg_xml_lite:parse_file(FileName),
   [{pi, _},{xml,{templates,[],[A]}}] = V,
   {template,[{"name","template1"}],Boxes} = A,   % select the template that matches the 2nd half of the module name
    {Env3, BoxRecords} = boxParse(Env2, Boxes),           % tear off Box Definitions for a template
    Env4 = loadBoxRecords(Env3,FilePart,BoxRecords),
 %  {box,BoxList,Objects} = BoxDef, % repeat for each box definition in the template
  %  [{Name,Value} | MoreValues] = BoxList, % repeat for each value of a box record
  Env4.
 
  
%% Parse out the descriptions of the boxes with their tags 

boxParse(Env, [BoxDef|MoreBoxes]) ->
  Record = #box{},
  {box,BoxItem,Objects} = BoxDef,
  BoxName = getBoxName(BoxItem),
  objectParse(Env, Objects, BoxName),
  Record2 = setBoxFields(Record,BoxItem),
  boxParse(Env, MoreBoxes, [Record2]).

%% parse out the values for the Box record and return a box record
  
boxParse(Env,[],Records) ->
  {Env, Records};
boxParse(Env, [BoxFields|MoreFields],Records) ->
  Record = #box{},
  {box,BoxItem,Objects} = BoxFields,  
  BoxName = getBoxName(BoxItem),
  Env2 = objectParse(Env, Objects, BoxName),
  Record2 = setBoxFields(Record,BoxItem),
  boxParse(Env2, MoreFields, Records ++ [Record2]). 

%% parse out each attribute & value for the box

setBoxFields(Record, []) ->
  Record;
setBoxFields(Record, [Field|MoreFields]) ->
  {Name,Value} = Field,
  Record2 = setBoxFields(Record,Name, Value),
  setBoxFields( Record2, MoreFields).
 
%% set the attributes of the box record
    
setBoxFields(Record,Name, Value ) ->
_RecordOut = case Name of
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

%% get the box name out of the box list

getBoxName(BoxItem) ->
  [{"name", BoxName}] = lists:filter(fun(X) -> case catch({"name", _Name} = X ) of
            {'EXIT',_Why} -> false; 
            _ -> true end 
          end, BoxItem),
          BoxName.
           
%% load boxes into the dict of the env ... key = {template_atom, box_name}

loadBoxRecords(Env,_Template,BoxRecords) ->
  Dict = Env#env.dict,
  DictNew = loadBoxRecords(Dict, Env, Env#env.template, BoxRecords),
  Env#env{dict = DictNew}.
  
loadBoxRecords(Dict, _, _, []) ->
  Dict;
loadBoxRecords(Dict, Env, Tmplt,[Rec|MoreRecs]) ->
  Key = {Tmplt, Rec#box.name},
  Dict2 = dict:store(Key, Rec, Dict),
  loadBoxRecords(Dict2, Env, Tmplt, MoreRecs).

%% Parse out the descriptions of the boxes with their tags 
  
objectParse(Env, [], _BoxName) ->
  Env;
objectParse(Env, [ObjDef|MoreObjs], BoxName) ->
  Record = #object{},
  {obj,ObjList,Tags} = ObjDef,
  Record2 = setObjFields(Record,ObjList),
  Env2 = tagParse(Env, Tags, BoxName, Record2#object.name),
  Env3 = putObjectRecord(Env2, Record2, BoxName, Record2#object.name),
  objectParse(Env3, MoreObjs,  BoxName).


%% parse out each attribute & value for the obj

setObjFields(Record, []) ->
  Record;
setObjFields(Record, [Field|MoreFields]) ->
  {Name,Value} = Field,
  Record2 = setObjFields(Record,Name, Value),
  setObjFields( Record2, MoreFields).
  
%% set the attributes of the obj record
    
setObjFields(Record,Name, Value ) ->
_RecordOut = case Name of
  "name" -> Record#object{ name = list_to_atom(Value)};
  "paraIndent" ->  Record#object{ paraIndent = list_to_integer(Value)}
end.

%% put the object record in the Env dictionary. Key is {template-atom, Box-name, Object-name}

putObjectRecord(Env, Record, BoxName, ObjName) ->
   Dict = Env#env.dict,
   Key = {Env#env.template, BoxName, ObjName},
   DictNew = dict:store(Key, Record, Dict),
   Env#env{dict = DictNew}.

%% Parse out the descriptions of the boxes with their tags 
  
tagParse(Env, [], _BoxName, _ObjectName) ->
  Env;
tagParse(Env, [TagDef|MoreTags], BoxName, ObjectName) ->
  Record = #tagMap{},
  {tag, TagList, _} = TagDef,
  Record2 = setTagFields(Record,TagList),
  Env2 = putTagRecord(Env, Record2, BoxName, ObjectName),
  tagParse(Env2, MoreTags,  BoxName, ObjectName).
  
%% parse out each attribute & value for the tqag

setTagFields(Record, []) ->
  Record;
setTagFields(Record, [Field|MoreFields]) ->
  {Name,Value} = Field,
  Record2 = setTagFields(Record,Name, Value),
  setTagFields( Record2, MoreFields).

%% set the attributes of the tag record
    
setTagFields(Record,Name, Value ) ->
_RecordOut = case Name of
  "name" -> Record#tagMap{ name = list_to_atom(Value)};
  "font" -> Record#tagMap{ font = Value};
  "color" -> Record#tagMap{ color = list_to_atom(Value)};
  "break" -> Record#tagMap{ break = list_to_atom(Value)};
  "voff" ->  Record#tagMap{ voff = list_to_integer(Value)};
  "size" ->  Record#tagMap{ size = list_to_integer(Value)}
end.

%% put the object record in the Env dictionary. Key is {template-atom, Box-name, Object-name, Tag-name}

putTagRecord(Env, Record, BoxName, ObjName) ->
   Dict = Env#env.dict,
   Key = {Env#env.template, BoxName, ObjName, Record#tagMap.name},
   DictNew = dict:store(Key, Record, Dict),
   Env#env{dict = DictNew}.
   
   
box(Env, Template, Box) ->
   Dict = Env#env.dict,
   dict:fetch({Template, Box}, Dict).
  
     
tag_map(Env, Template, Box, Object) ->
   Dict = Env#env.dict,
   _AccOut = dict:fold(fun(Key, Value, Acc) ->
             case catch( {Template, Box, Object, _Tag} = Key) of
               {'EXIT', _Why} -> Acc;
               _ -> Acc ++ Value
               end
               end, [],Dict). 
          
handler(Box, TagMap, _Args, Data, Env) ->
    case Box#box.bg of 
      default ->      eg_block:inner_block(Env#env.pdf,  Data, 
                              Box#box.x, Box#box.y, Box#box.width, Box#box.fontSize, 
                              Box#box.leading, Box#box.lines, Box#box.justify, TagMap);
      _ ->            eg_block:colored_inner_block(Env#env.pdf, Box#box.bg, Data, 
                              Box#box.x, Box#box.y, Box#box.width, Box#box.fontSize, 
                              Box#box.leading, Box#box.lines, Box#box.justify, TagMap)
                            end,
    Env.
    

