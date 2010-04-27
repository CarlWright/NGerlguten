-module (template1_one).

-include("../../include/eg.hrl").

-export ([on_instanciation/2, box/1]).

on_instanciation(Page,PDF) ->
  {Page, PDF}.
  
box(Box) ->
  
  Box.