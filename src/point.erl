%%%-------------------------------------------------------------------
%%% @author mb189v
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Aug 2019 3:27 PM
%%%-------------------------------------------------------------------
-module(point).
-author("mb189v").

%% API
-export([point_plus_vector/2]).

point_plus_vector({Px, Py, Pz}, {Vi, Vj, Vk}) ->
  {Px + Vi, Py + Vj, Pz + Vk}.
