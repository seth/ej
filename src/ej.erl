% License: Apache License, Version 2.0
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
% 
%     http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
%% @author Seth Falcon <seth@userprimary.net>
%% @copyright Copyright 2011 Seth Falcon
%%
%% @doc Tools for working with Erlang terms representing JSON.
%%
%% The ej module is intended to make it easy to work with the Erlang
%% structure used by `mochijson2' to represent JSON.  You can use
%% `ej:get' to walk an object and return a particular value, or
%% `ej:set' to update a value.
%%
%% @end

-module(ej).
-author('Seth Falcon <seth@userprimary.net').
-export([
         get/2,
         set/3
         ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Extract a value from `Obj'
%%
%% `Keys' is a tuple specifying a path into the JSON structure.  Each
%% string or binary element of `Keys' will act like a Javascript
%% property lookup.  Elements of JSON arrays can be accessed by
%% including an integer as an element of `Keys'.  In addition, the
%% atoms `` 'first' '' and `` 'last' '' can be used to access the
%% first and last elements of a list, respectively.
%%
get(Keys, Obj) when is_tuple(Keys) ->
   get0(tuple_to_list(Keys), Obj).

get0([Key | Rest], Obj) ->
    case get_value(Key, Obj) of
        undefined -> undefined;
        AValue -> get0(Rest, AValue)
    end;
get0([], Value) ->
    Value.


get_value(Key, Obj) when is_binary(Key) ->
    case Obj of
        {struct, L} ->
            get_value(Key, L);
        PL=[{_, _}|_T] ->
            proplists:get_value(Key, PL);
        [_H|_T] ->
            undefined
    end;
get_value(Key, Obj) when is_list(Key) ->
    get_value(iolist_to_binary(Key), Obj);
get_value(first, [H|_T]) ->
    H;
get_value(last, List=[_H|_T]) ->
    lists:last(List);
get_value(Index, List=[_H|_T]) when is_integer(Index) ->
    lists:nth(Index, List);
get_value(Index, Obj) ->
    erlang:error({index_for_non_list, {Index, Obj}}).

%% @doc Set a value in `Obj'
%%
%% Replaces the value at the path specified by `Keys' with `Value' and
%% returns the new structure.
set(Keys, Obj, Value) when is_tuple(Keys) ->
   set0(lists:reverse(tuple_to_list(Keys)), Obj, Value).

% to set a value, we get the value of the path one-level up, and then
% set.  So we want to set values popping off the end of the path list.
% Is there a better way to do this than calling reverse on Rest for
% each key in path?
set0([Key | Rest], Obj, Value) ->
    TheObj = get0(lists:reverse(Rest), Obj),
    set0(Rest, Obj, set_value(Key, TheObj, Value));
set0([], _Obj, Value) ->
    Value.


set_value(Key, Obj, Value) when is_binary(Key) ->
    case Obj of
        {struct, L} ->
            set_value(Key, L, Value);
        PL=[{_, _}|_T] ->
            {struct, lists:keyreplace(Key, 1, PL, {Key, Value})};
        [] ->
            undefined;
        _Else ->
            Obj
    end;
set_value(Key, Obj, Value) when is_list(Key) ->
    set_value(iolist_to_binary(Key), Obj, Value);
set_value(first, List=[_H|_T], Value) ->
    [Value | List];
set_value(last, List=[_H|_T], Value) ->
    List ++ [Value].


-ifdef(TEST).

ej_test_() ->
{setup,
 fun() ->
         {ok, [Widget]} = file:consult("../test/widget.terms"),
         {ok, [Glossary]} = file:consult("../test/glossary.terms"),
         ObjList = {struct, [{<<"objects">>,
                              [ {struct, [{<<"id">>, I}]} ||
                                  I <- lists:seq(1, 5) ]}]},
         {Widget, Glossary, ObjList}
 end,
 fun({Widget, Glossary, ObjList}) ->
         [{"ej:get",
           [
            ?_assertMatch({struct, [{_, _}|_]}, ej:get({"widget"}, Widget)),
            ?_assertEqual(<<"1">>, ej:get({"widget", "version"}, Widget)),
            ?_assertEqual(250, ej:get({"widget", "image", "hOffset"}, Widget)),
            ?_assertEqual([1,2,3,4,5], ej:get({"widget", "values"}, Widget)),
            ?_assertEqual(2, ej:get({"widget", "values", 2}, Widget)),
            ?_assertEqual(4, ej:get({"widget", "values", 4}, Widget)),
            ?_assertEqual(1, ej:get({"widget", "values", first}, Widget)),
            ?_assertEqual(5, ej:get({"widget", "values", last}, Widget)),
            ?_assertEqual({struct, [{<<"id">>, 5}]},
                          ej:get({<<"objects">>, last}, ObjList)),
            ?_assertEqual({struct, [{<<"id">>, 1}]},
                          ej:get({<<"objects">>, first}, ObjList)),
            ?_assertEqual(undefined, ej:get({"fizzle"}, Widget)),
            ?_assertEqual(undefined, ej:get({"widget", "fizzle"}, Widget)),
            ?_assertEqual(undefined,
                          ej:get({"widget", "values", "fizzle"},Widget)),

            ?_assertEqual(<<"SGML">>,
                          ej:get({"glossary", "GlossDiv", "GlossList",
                                  "GlossEntry", "Acronym"}, Glossary)),

            ?_assertEqual(undefined,
                          ej:get({"glossary", "GlossDiv", "GlossList",
                                  "GlossEntry", "fizzle"}, Glossary)),

            ?_assertException(error, {index_for_non_list, _},
                              ej:get({"glossary", "GlossDiv", "GlossList",
                                      "GlossEntry", 1}, Glossary)),

            ?_assertException(error, {index_for_non_list, _},
                              ej:get({"glossary", "title", 1}, Glossary))]},

          {"ej:set top-level",
           fun() ->
                   NewVal = <<"2">>,
                   OrigCheck = ej:get({"widget", "image", "name"}, Widget),
                   Widget1 = ej:set({"widget", "version"}, Widget, NewVal),
                   ?assertEqual(NewVal, ej:get({"widget", "version"}, Widget1)),
                   ?assertEqual(OrigCheck,
                                ej:get({"widget", "image", "name"}, Widget1))
           end},

          {"ej:set nested",
           fun() ->
                   NewVal = <<"JSON">>,
                   Path = {"glossary", "GlossDiv", "GlossList", "GlossEntry",
                           "ID"},
                   Unchanged = ej:get({"glossary", "GlossDiv", "GlossList",
                                       "GlossEntry", "SortAs"}, Glossary),
                   Glossary1 = ej:set(Path, Glossary, NewVal),
                   ?assertEqual(NewVal, ej:get(Path, Glossary1)),
                   ?assertEqual(Unchanged, ej:get({"glossary", "GlossDiv",
                                                   "GlossList", "GlossEntry",
                                                   "SortAs"}, Glossary1))
           end}
         ]
 end
}.

-endif.

