-module(ej).
-author('Seth Falcon <seth@userprimary.net').

-export([
         get/2,
         set/3
         ]).

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%-endif.

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


set(Keys, Obj, Value) when is_tuple(Keys) ->
   set0(lists:reverse(tuple_to_list(Keys)), Obj, Value).

set0([Key | Rest], Obj, Value) ->
    TheObj = get0(lists:reverse(Rest), Obj),
    NewVal = set_value(Key, TheObj, Value),
    NewObj = Obj,
    % ?debugVal(TheObj),
    % ?debugVal(NewVal),
    % ?debugVal(NewObj),
    set0(Rest, NewObj, NewVal);
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
ej_get_test_() ->
    {ok, [Widget]} = file:consult("../test/widget.terms"),
    {ok, [Glossary]} = file:consult("../test/glossary.terms"),
    ObjList = {struct, [{<<"objects">>,
                         [ {struct, [{<<"id">>, I}]} ||
                             I <- lists:seq(1, 5) ]}]},
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
     ?_assertEqual(undefined, ej:get({"widget", "values", "fizzle"}, Widget)),

     ?_assertEqual(<<"SGML">>, ej:get({"glossary", "GlossDiv", "GlossList",
                                       "GlossEntry", "Acronym"}, Glossary)),

     ?_assertEqual(undefined, ej:get({"glossary", "GlossDiv", "GlossList",
                                      "GlossEntry", "fizzle"}, Glossary)),

     ?_assertException(error, {index_for_non_list, _},
                       ej:get({"glossary", "GlossDiv", "GlossList",
                               "GlossEntry", 1}, Glossary)),

     ?_assertException(error, {index_for_non_list, _},
                       ej:get({"glossary", "title", 1}, Glossary))

    ].

ej_set_test_() ->
    {ok, [Widget]} = file:consult("../test/widget.terms"),
    {ok, [Glossary]} = file:consult("../test/glossary.terms"),
    [
     {"set top-level simple",
      fun() ->
              NewVal = <<"2">>,
              Widget1 = ej:set({"widget", "version"}, Widget, NewVal),
              ?assertEqual(NewVal, ej:get({"widget", "version"}, Widget1))
      end},

     {"set nested",
      fun() ->
              NewVal = <<"JSON">>,
              Glossary1 = ej:set({"glossary", "GlossDiv", "GlossList", "GlossEntry",
                                  "ID"}, Glossary, NewVal),
              ?assertEqual(NewVal, ej:get({"glossary", "GlossDiv", "GlossList", "GlossEntry",
                                  "ID"}, Glossary1))
      end},

     {"set nested glossary",
      fun() ->
              Glossary1 = ej:set({"glossary", "GlossDiv", "title"}, Glossary, <<"Foo">>),
              ?assertEqual(<<"Foo">>, ej:get({"glossary", "GlossDiv", "title"}, Glossary1))
      end}


    ].

ej_test_() ->
    % {ok, [Widget]} = file:consult("../test/widget.terms"),
    [
     {"more here",
     fun() ->
             ok
     end}
    ].
% eunit tests
-endif.

