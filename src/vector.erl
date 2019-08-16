%%%-------------------------------------------------------------------
%%% @author mb189v
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Aug 2019 3:27 PM
%%%-------------------------------------------------------------------
-module(vector).
-author("mb189v").

%% API
-export([multiply/2]).

multiply({Vi, Vj, Vk}, Number) ->
  {Vi * Number, Vj * Number, Vk * Number}.