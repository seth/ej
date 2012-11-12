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
%% @copyright Copyright 2011-2012 Seth Falcon
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
         get/3,
         set/3,
         set_p/3,
         delete/2,
         valid/2
         ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("ej.hrl").

-export_type([json_object/0,
              json_plist/0,
              json_term/0]).

%% @doc Extract a value from `Obj'
%%
%% `Keys' is a tuple specifying a path into the JSON structure.  Each
%% string or binary element of `Keys' will act like a Javascript
%% property lookup.  Elements of JSON arrays can be accessed by
%% including an integer as an element of `Keys'.  In addition, the
%% atoms `` 'first' '' and `` 'last' '' can be used to access the
%% first and last elements of a list, respectively.
%%
-spec(get(key_tuple(), json_object() | json_plist()) -> json_term() | undefined).

get({}, _Obj) ->
    undefined;
get(Keys, Obj) when is_tuple(Keys) ->
   get0(tuple_to_list(Keys), Obj).

%% @doc same as get/2, but returns `Default' if the specified value was not found.
-spec get(key_tuple(), json_object() | json_plist(), json_term()) -> json_term().
get({}, _Obj, Default)  ->
    Default;
get(Keys, Obj, Default) when is_tuple(Keys) ->
    case get(Keys, Obj) of
        undefined ->
            Default;
        Value ->
            Value
    end.

get0([Key | Rest], Obj) ->
    case get_value(Key, Obj) of
        undefined -> undefined;
        AValue -> get0(Rest, AValue)
    end;
get0([], {from_select, Value}) ->
    Value;
get0([], Value) ->
    Value.


%% -define(IS_OBJECT(Obj), (is_tuple(Obj) andalso (1 =:= tuple_size(Obj)
%%                                                orelse
%%                                                struct =:= element(1, Obj)))).

get_value(Key, Obj) when is_list(Key) ->
    get_value(iolist_to_binary(Key), Obj);
get_value(Key, {struct, L}) when is_binary(Key) orelse filter =:= element(1, Key) ->
    get_value(Key, L);
get_value(Key, {L}) when is_binary(Key); is_tuple(Key) -> % alt form
    get_value(Key, L);
get_value(Key, {from_select, []}) when is_binary(Key) ->
    undefined;
get_value(Key, {from_select, List}) when is_binary(Key) ->
    lists:flatten([get_value(Key, L) || L <- List]);
get_value(Key, PL=[{_, _}|_T]) when is_binary(Key) ->
    proplists:get_value(Key, PL);
get_value(Key, [_H|_T]) when is_binary(Key) ->
    undefined;
get_value(Key, []) when is_binary(Key) ->
    undefined;
get_value(first, [H|_T]) ->
    H;
get_value(last, List=[_H|_T]) ->
    lists:last(List);
get_value(Index, List=[_H|_T]) when is_integer(Index) ->
    lists:nth(Index, List);
get_value({select, KeyValue}, List=[_H|_T]) when is_tuple(KeyValue) orelse KeyValue =:= all ->
    {from_select, matching_array_elements(KeyValue, List)};
get_value(Index, Obj) ->
    erlang:error({index_for_non_list, {Index, Obj}}).

as_binary(Key) when is_binary(Key) ->
    Key;
as_binary(Key) when is_list(Key) ->
    iolist_to_binary(Key);
as_binary(Key) when is_tuple(Key) ->
    Key;
as_binary(Key) when is_integer(Key) orelse is_atom(Key) ->
    Key.

matching_array_elements(all, List) ->
    List;
matching_array_elements(CompKey, List) ->
    lists:filter(fun(E) -> matching_element(CompKey, E) end, List).

matching_element({K, V}, {struct, E}) ->
    Value = as_binary(V),
    case proplists:get_value(as_binary(K), E) of
      Value -> true;
      _ -> false
    end;
matching_element({K, V}, {E}) ->
    Value = as_binary(V),
    case proplists:get_value(as_binary(K), E) of
      Value -> true;
      _ -> false
    end;
matching_element(Key, E) ->
    erlang:error({error_matching_element, {Key, E}}).

%% @doc Set a value in `Obj'
%%
%% Replaces the value at the path specified by `Keys' with `Value' and
%% returns the new structure.  If `Value' is the atom `EJ_DELETE',
%% then the path specified by `Keys' is removed (but see `delete/2').
%%
-spec(set(key_tuple(), json_object(), json_term()) -> json_term()).
set(Keys, Obj, Value) when is_tuple(Keys) ->
    set0([ as_binary(X) || X <- tuple_to_list(Keys) ], Obj, Value, []).

%% @doc Set a value in `Obj' and create missing intermediate
%%      nodes if need be.
%%
%% This resembles the -p option in mkdir. If the intermediate
%% elements in the structure are missing, then they are created.
%% This is useful when creating complex JSON structures from scratch.
%%
%% The arguments are the same as for `set'.
%%
-spec(set_p(key_tuple(), json_object(), json_term()) -> json_term()).
set_p(Keys, Obj, Value) when is_tuple(Keys) ->
    set0([ as_binary(X) || X <- tuple_to_list(Keys) ], Obj, Value, [create_missing]).

set0([], _, Value, _) ->
    Value;
set0([Key | Rest], {struct, P}, Value, Options)
  when is_binary(Key) orelse Key == 'EJ_DELETE' ->
    case {get_value(Key, P), length(Rest), Value, proplists:get_value(create_missing, Options)} of
        %% Is matchen when we creating new nested structures
        {undefined, _, _, true} ->
            {struct, lists:keystore(Key, 1, P,
                                    {Key, set0(Rest, {struct, []}, Value, Options)})};
        {undefined, Len, _, _} when Len > 0 ->
            erlang:error({no_path, Key});
        {_, Len, 'EJ_DELETE', _} when Len == 0 ->
            {struct, lists:keydelete(Key, 1, P)};
        {Downstream, _, _, _} ->
            {struct, lists:keystore(Key, 1, P,
                                    {Key, set0(Rest, Downstream, Value, [{make_object, fun make_struct_object/1} |Options])})}
    end;
set0([Key | Rest], {P}, Value, Options) % clean this up? alt form
  when is_binary(Key) orelse Key == 'EJ_DELETE' ->
    case {get_value(Key, P), length(Rest), Value, proplists:get_value(create_missing, Options)} of
        {undefined, _, _, true} ->
            {lists:keystore(Key, 1, P,
                            {Key, set0(Rest, {[]}, Value, Options)})};
        {undefined, Len, _, _} when Len > 0 ->
            erlang:error({no_path, Key});
        {_, Len, 'EJ_DELETE', _} when Len == 0 ->
            {lists:keydelete(Key, 1, P)};
        {Downstream, _, _, _} ->
            {lists:keystore(Key, 1, P,
                            {Key, set0(Rest, Downstream, Value, [{make_object, fun make_object/1} |Options])})}
    end;
set0([new | []], P, Value, _Options) when is_list(P) ->
    [Value|P];
set0([{select, Key = {_,_}}], P, 'EJ_DELETE', _Options) when is_list(P) ->
    lists:filter(fun(E) -> not matching_element(Key, E) end, P);
set0([{_,_}], P, Object, _Options) when not is_tuple(Object) ->
    erlang:error({replacing_object_with_value, {P, Object}});
set0(Key = [{select, {_,_}} | _], {struct, P}, Value, Options) ->
    set0(Key, P, Value, [{make_object, fun make_struct_object/1} | Options]);
set0(Key = [{select, {_,_}} | _], {P}, Value, Options) ->
    set0(Key, P, Value, [{make_object, fun make_object/1} | Options]);
set0([ {select, Filter = {K,_}} | Rest], P, Value, Options) when is_list(P) ->
    MakeObject = proplists:get_value(make_object, Options),
    {Existed, Res} = lists:foldl(fun(E, {WhetherFound, Acc}) ->
        case matching_element(Filter, E) of
            true -> 
                ChildElems = object_list(set0(Rest, E, Value, Options)),
                Child = MakeObject(lists:keystore(as_binary(K), 1, ChildElems, composite_key_as_binary(Filter))),
                {true, [Child | Acc]};
            false -> 
                {WhetherFound, [E | Acc]}
        end
    end, {false, []}, P),
    case {Existed, proplists:get_value(create_missing, Options)} of
        {true, _} ->
            lists:reverse(Res);
        {false, true} ->
            ChildElems = object_list(set0(Rest, MakeObject([]), Value, Options)),
            Child = lists:keystore(K, 1, ChildElems, composite_key_as_binary(Filter)),
            [MakeObject(Child) | lists:reverse(Res)];
        {false, _} ->
            erlang:error({no_path, Filter})
    end;
set0([Idx | Rest], P, Value, Options)
  when is_integer(Idx) orelse is_atom(Idx); is_list(P) ->
    case {get_value(Idx, P), length(Rest), Value} of
        {undefined, Len, _} when Len > 0 ->
            erlang:error({no_path, Idx});
        {_, Len, 'EJ_DELETE'} when Len == 0 ->
            set_nth(Idx, P, 'EJ_DELETE');
        {Downstream, _, _} ->
            set_nth(Idx, P, set0(Rest, Downstream, Value, Options))
end.

object_list({struct, L}) ->
    L;
object_list({L}) ->
    L.

make_object(L) ->
    {L}.

make_struct_object(L) ->
    {struct, L}.

composite_key_as_binary({K, V}) -> {as_binary(K), as_binary(V)}.

set_nth(first, [_H|T], 'EJ_DELETE') ->
    T;
set_nth(first, [_H|T], V) ->
    [V|T];
set_nth(last, L, 'EJ_DELETE') ->
    [_H|T] = lists:reverse(L),
    lists:reverse(T);
set_nth(last, L, V) ->
    [_H|T] = lists:reverse(L),
    lists:reverse([V|T]);
set_nth(N, L, 'EJ_DELETE') ->
    {L1, [_H|L2]} = lists:split(N - 1, L),
    lists:concat([L1, L2]);
set_nth(N, L, V) ->
    {L1, [_H|L2]} = lists:split(N - 1, L),
    lists:concat([L1, [V|L2]]).


% TODO: support setting list elements as well as a means to add new
% elements to a list.

%% @doc Remove the item specified by `Keys'.
-spec(delete(key_tuple(), json_object()) -> json_object()).

delete(Keys, Obj) when is_tuple(Keys) ->
    set0([ as_binary(X) || X <- tuple_to_list(Keys) ], Obj, 'EJ_DELETE', []).

%% valid - JSON term validation via spec

%% context threaded through validity checking
-record(spec_ctx, {
          %% List of keys keeping track of where we are in a nested
          %% JSON object.
          path = [] :: [binary()],
          %% Future use: use this to collect errors so that validation
          %% can report a list of errors rather than just the first
          %% one.
          errors = [] :: [term()]
         }).

%% An re module regex (compiled) and a message that will be
%% returned when nomatch is triggered.
-type ej_string_match() :: {'string_match', {re:mp(), _}}.

%% User supplied validation function. This must be an arity 1 fun that
%% will be given the value and should return 'ok' if the value is
%% good. Any other return is treated as an invalid result. The type
%% name describes the expected type of the value. We might want to
%% change this or remove it if we want to support a notion of 'any_of'
%% matching. The advantage for now is that we can auto-generate a
%% better missing message.
-type ej_fun_match() :: {fun_match, {fun((json_term()) -> ok | error),
                                        ej_json_type_name(), _}}.

%% Map a value spec over each element of an array value.
-type ej_array_map() :: {array_map, ej_json_val_spec()}.

%% Walk the key/value pairs of a JSON object and execute the
%% corresponding key and value specs for each pair.
-type ej_object_map() :: {object_map, {{keys, ej_json_val_spec()},
                                       {values, ej_json_val_spec()}}}.

-type ej_json_spec() :: {[ej_json_spec_rule()]} | ej_object_map().
-type ej_json_spec_rule() :: {ej_json_key_spec(), ej_json_val_spec()}.
-type ej_json_key_spec() :: binary() | {opt, binary()}.
-type ej_json_val_spec() :: binary()             |
                            ej_json_type_name()  |
                            ej_string_match()    |
                            ej_fun_match()       |
                            ej_array_map()       |
                            ej_object_map()      |
                            {[ej_json_val_spec()]}.

-spec valid(Spec :: ej_json_spec(), Obj:: json_object()) -> ok | #ej_invalid{}.
%% @doc Validate JSON terms. Validity is determined by the
%% `ej_json_spec()` provided which has the shape of EJSON terms but
%% with keys and values describing what is expected. `Obj' is the
%% EJSON term to be validated. This function will return `ok' if all
%% validation rules succeed and a `#ej_invalid{}' record when the
%% first failure is encountered (validation specs are processed in
%% order, depth first).  NOTE: this function is experimental and the
%% API and definition of specs is subject to change.
valid({object_map, _}=Spec, Obj={OL}) when is_list(OL) ->
    check_value_spec(<<"no_key">>, Spec, Obj, #spec_ctx{});
valid({L}, Obj={OL}) when is_list(L) andalso is_list(OL) ->
    valid(L, Obj, #spec_ctx{});
valid({L}, Obj) when is_list(L) ->
    #ej_invalid{type = json_type, key = undefined,
                expected_type = object,
                found_type = json_type(Obj),
                found = Obj}.

valid([{{Opt, Key}, ValSpec}|Rest], Obj, Ctx = #spec_ctx{path = Path} = Ctx)
  when is_binary(Key) andalso (Opt =:= opt orelse Opt =:= req) ->
    case {Opt, ej:get({Key}, Obj)} of
        {opt, undefined} ->
            valid(Rest, Obj, Ctx);
        {req, undefined} ->
            #ej_invalid{type = missing, key = make_key(Key, Path),
                        expected_type = type_from_spec(ValSpec)};
        {_, Val} ->
            case check_value_spec(Key, ValSpec, Val, Ctx) of
                ok ->
                    valid(Rest, Obj, Ctx);
                Error ->
                    Error
            end
    end;
valid([{Key, ValSpec}|Rest], Obj, #spec_ctx{} = Ctx) when is_binary(Key) ->
    %% required key literal
    valid([{{req, Key}, ValSpec}|Rest], Obj, Ctx);
valid([], _Obj, _Ctx) ->
    ok.

-spec make_key(Key :: binary(), Path :: [binary()]) -> binary().
%% Given a key and a list of keys in `Path' indicating the traversal
%% path, build a JSON-style key separated by dots.
make_key(Key, Path) ->
    join_path(make_path(Key, Path)).

make_path(Key, Path) ->
    list_to_tuple(lists:reverse([Key | Path])).

join_path(Path) ->
    join_bins(tuple_to_list(Path), <<".">>).

%% Return a JSON type name to be used as the expected_type based on a
%% value spec. If no type can be determined or any type is accepted,
%% 'any_value' should be returned.
type_from_spec({string_match, _}) ->
    string;
type_from_spec({array_map, _}) ->
    array;
type_from_spec({object_map, _}) ->
    object;
type_from_spec({fun_match, {_, Type, _}}) ->
    Type;
type_from_spec(Literal) when is_binary(Literal) ->
    string;
type_from_spec(Literal) when is_integer(Literal) orelse is_float(Literal) ->
    number;
type_from_spec({L}) when is_list(L) ->
    object;
type_from_spec({any_of, {Specs, _ErrorMsg}}) ->
    type_from_any_of(Specs);
type_from_spec(Type) when Type =:= string;
                          Type =:= number;
                          Type =:= boolean;
                          Type =:= array;
                          Type =:= object;
                          Type =:= null;
                          Type =:= any_value ->
    Type;
type_from_spec(Type) ->
    error({unknown_spec, type_from_spec, Type}).

%% Attempt to find a type for an any_of spec. If all the containing
%% specs have the same type, return that; otherwise, return
%% 'any_value' as the type placeholder.
type_from_any_of([]) ->
    any_value;
type_from_any_of(Specs) ->
    type_from_any_of(Specs, unset).

type_from_any_of([Spec | Rest], Ans) ->
    CurType = type_from_spec(Spec),
    case Ans of
        PrevType when PrevType =:= unset;
                      PrevType =:= CurType ->
            type_from_any_of(Rest, CurType);
        _DiffType ->
            any_value
    end;
type_from_any_of([], Ans) ->
    Ans.

%% Map an EJSON term to JSON type name.
json_type(Val) when is_binary(Val) ->
    string;
json_type({L}) when is_list(L) ->
    object;
json_type(L) when is_list(L) ->
    array;
json_type(null) ->
    null;
json_type(Bool) when Bool =:= true; Bool =:= false ->
    boolean;
json_type(N) when is_integer(N) orelse is_float(N) ->
    number.

check_value_spec(Key, {L}, Val={V}, #spec_ctx{path = Path} = Ctx) when is_list(L) andalso is_list(V) ->
    %% traverse nested spec here
    valid(L, Val, Ctx#spec_ctx{path = [Key|Path]});
check_value_spec(Key, {L}, Val, #spec_ctx{path = Path}) when is_list(L) ->
    %% was expecting nested spec, found non-object
    #ej_invalid{type = json_type, key = make_key(Key, Path),
                expected_type = object,
                found = Val,
                found_type = json_type(Val)};
check_value_spec(Key, {string_match, {Regex, Msg}}, Val, #spec_ctx{path = Path}) when is_binary(Val) ->
    %% string_match
    case re:run(Val, Regex) of
        nomatch ->
            #ej_invalid{type = string_match, key = make_key(Key, Path),
                        expected_type = string,
                        found = Val,
                        found_type = string,
                        msg = Msg};
        {match, _} ->
            ok
    end;
check_value_spec(Key, {string_match, _}, Val, #spec_ctx{path = Path}) ->
    %% expected string for string_match, got wrong type
    #ej_invalid{type = json_type, key = make_key(Key, Path),
                expected_type = string,
                found_type = json_type(Val),
                found = Val};

check_value_spec(Key, {fun_match, {Fun, Type, Msg}}, Val, #spec_ctx{path = Path}) ->
    %% user supplied fun
    FoundType = json_type(Val),
    case FoundType =:= Type of
        false ->
            #ej_invalid{type = json_type, key = make_key(Key, Path),
                        expected_type = Type,
                        found_type = FoundType,
                        found = Val};
        true ->
            case Fun(Val) of
                ok ->
                    ok;
                _ ->
                    #ej_invalid{type = fun_match, key = make_key(Key, Path),
                                expected_type = Type,
                                found = Val,
                                found_type = json_type(Val),
                                msg = Msg}
            end
    end;

check_value_spec(Key, {array_map, ItemSpec}, Val, #spec_ctx{path = Path}) when is_list(Val) ->
    case do_array_map(ItemSpec, Val) of
        ok ->
            ok;
        {bad_item, InvalidItem} ->
            InvalidItem#ej_invalid{type = array_elt,
                                   key = make_key(Key, Path)}
    end;
check_value_spec(Key, {array_map, _ItemSpec}, Val, #spec_ctx{path = Path}) ->
    %% expected an array for array_map, found wrong type
    #ej_invalid{type = json_type, key = make_key(Key, Path),
                expected_type = array,
                found_type = json_type(Val),
                found = Val};

check_value_spec(Key, {object_map, {{keys, KeySpec}, {values, ValSpec}}},
                 Val={L}, #spec_ctx{path = Path}) when is_list(L) ->
    case do_object_map(KeySpec, ValSpec, Val) of
        ok ->
            ok;
        {bad_item, Type, InvalidItem} ->
            InvalidItem#ej_invalid{type = Type,
                                   key = make_key(Key, Path)}
    end;
check_value_spec(Key, {object_map, _ItemSpec}, Val, #spec_ctx{path = Path}) ->
    %% expected an object for object_map, found wrong type
    #ej_invalid{type = json_type, key = make_key(Key, Path),
                expected_type = object,
                found_type = json_type(Val),
                found = Val};

check_value_spec(Key, {any_of, {Specs, ErrorMsg}}, Val, Ctx) ->
    check_any_of_value_specs(Key, Val, Ctx, Specs, ErrorMsg);

check_value_spec(_Key, any_value, _Val, _Ctx) ->
    ok;

check_value_spec(_Key, string, Val, _Ctx) when is_binary(Val) ->
    ok;
check_value_spec(Key, string, Val, #spec_ctx{path = Path}) ->
    invalid_for_type(string, Val, Key, Path);

check_value_spec(_Key, object, {VL}, _Ctx) when is_list(VL) ->
    ok;
check_value_spec(Key, object, Val, #spec_ctx{path = Path}) ->
    invalid_for_type(object, Val, Key, Path);

check_value_spec(_Key, number, Val, _Ctx) when is_number(Val) ->
    ok;
check_value_spec(Key, number, Val, #spec_ctx{path = Path}) ->
    invalid_for_type(number, Val, Key, Path);

check_value_spec(_Key, array, Val, _Ctx) when is_list(Val) ->
    ok;
check_value_spec(Key, array, Val, #spec_ctx{path = Path}) ->
    invalid_for_type(array, Val, Key, Path);

check_value_spec(_Key, null, null, _Ctx) ->
    ok;
check_value_spec(Key, null, Val, #spec_ctx{path = Path}) ->
    invalid_for_type(null, Val, Key, Path);

check_value_spec(_Key, boolean, Val, _Ctx) when Val =:= true; Val =:= false ->
    ok;
check_value_spec(Key, boolean, Val, #spec_ctx{path = Path}) ->
    invalid_for_type(boolean, Val, Key, Path);

check_value_spec(_Key, Val, Val, _Ctx) when is_binary(Val) ->
    %% exact match desired
    ok;
check_value_spec(Key, SpecVal, Val, #spec_ctx{path = Path}) when is_binary(SpecVal) ->
    %% exact match failed
    #ej_invalid{type = exact,
                key = make_key(Key, Path),
                found = Val,
                expected_type = string,
                found_type = json_type(Val),
                msg = SpecVal};
check_value_spec(Key, SpecVal, Val, #spec_ctx{path = Path}) ->
    %% catch all
    error({unknown_spec, SpecVal, {key, make_key(Key, Path)}, {value, Val}, {path, Path}}).


invalid_for_type(ExpectType, Val, Key, Path) ->
    #ej_invalid{type = json_type,
                expected_type = ExpectType,
                found_type = json_type(Val),
                found = Val,
                key = make_key(Key, Path)}.

do_array_map(_ItemSpec, []) ->
    ok;
do_array_map(ItemSpec, [Item|Rest]) ->
    %% FIXME: do we want to record element index?
    case check_value_spec(<<"item_fake_key">>, ItemSpec, Item, #spec_ctx{}) of
        ok ->
            do_array_map(ItemSpec, Rest);
        Error ->
            {bad_item, Error}
    end.

do_object_map(KeySpec, ValSpec, {L}) when is_list(L) ->
    do_object_map(KeySpec, ValSpec, L);
do_object_map(_KeySpec, _ValSpec, []) ->
    ok;
do_object_map(KeySpec, ValSpec, [{Key, Val}|Rest]) ->
    case check_value_spec(<<"item_fake_key_key">>, KeySpec, Key, #spec_ctx{}) of
        ok ->
            case check_value_spec(<<"item_fake_key_value">>, ValSpec, Val, #spec_ctx{}) of
                ok ->
                    do_object_map(KeySpec, ValSpec, Rest);
                ValueError ->
                    {bad_item, object_value, ValueError}
            end;
        KeyError ->
            {bad_item, object_key, KeyError}
    end.

check_any_of_value_specs(Key, Val, #spec_ctx{path = Path}, [], ErrorMsg) ->
    #ej_invalid{type = any_of,
                key = make_key(Key, Path),
                found = Val,
                expected_type = any_value,
                found_type = json_type(Val),
                msg = ErrorMsg};
check_any_of_value_specs(Key, Val, Ctx, [Spec1|OtherSpecs], ErrorMsg) ->
    case check_value_spec(Key, Spec1, Val, Ctx) of
        ok -> ok;
        _Error -> check_any_of_value_specs(Key, Val, Ctx, OtherSpecs, ErrorMsg)
    end.

join_bins([], _Sep) ->
    <<>>;
join_bins(Bins, Sep) when is_binary(Sep) ->
    join_bins(Bins, Sep, []).

join_bins([B], _Sep, Acc) ->
    iolist_to_binary(lists:reverse([B|Acc]));
join_bins([B|Rest], Sep, Acc) ->
    join_bins(Rest, Sep, [Sep, B | Acc]).


%% end valid
-ifdef(TEST).

ej_test_() ->
{setup,
 fun() ->
         {ok, [Widget]} = file:consult("../test/widget.terms"),
         {ok, [Glossary]} = file:consult("../test/glossary.terms"),
         {ok, [Menu]} = file:consult("../test/menu.terms"),
         ObjList = {struct, [{<<"objects">>,
                              [ {struct, [{<<"id">>, I}]} ||
                                  I <- lists:seq(1, 5) ]}]},
         {Widget, Glossary, Menu, ObjList}
 end,
 fun({Widget, Glossary, Menu, ObjList}) ->
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

            ?_assertEqual(undefined,
                          ej:get({"not_present"}, {[]})),

            ?_assertEqual(undefined,
                          ej:get({"not_present"}, {struct, []})),

            ?_assertEqual(undefined, ej:get({[]}, Widget)),

            ?_assertEqual(undefined, ej:get({}, Widget)),

            ?_assertException(error, {index_for_non_list, _},
                              ej:get({"glossary", "GlossDiv", "GlossList",
                                      "GlossEntry", 1}, Glossary)),

            ?_assertException(error, {index_for_non_list, _},
                              ej:get({"glossary", "title", 1}, Glossary))]},

          {"ej:get with default",
           [
            ?_assertEqual(<<"1">>, ej:get({"widget", "version"}, Widget, "you'll never see this default")),
            ?_assertEqual(<<"defaults rock">>, ej:get({"widget", "NOT_PRESENT"}, Widget, <<"defaults rock">>)),
            ?_assertEqual(<<"a default">>, ej:get({}, Widget, <<"a default">>)),
            ?_assertEqual(<<"a default">>, ej:get({[]}, Widget, <<"a default">>))
           ]},

          {"ej:get with json_plist",
           [
            ?_assertEqual(<<"1">>, ej:get({"a"}, [{<<"a">>, <<"1">>}])),
            ?_assertEqual(undefined, ej:get({"x"}, [{<<"a">>, <<"1">>}])),
            ?_assertEqual(undefined, ej:get({"x"}, []))
           ]},

          {"ej:get from array by matching key",
           fun() ->
              Path1 = {"menu", "popup", "menuitem", {select, {"value", "New"}}},
              ?assertMatch([{struct, [{<<"value">>,<<"New">>}|_]}], ej:get(Path1, Menu)),
              Path2 = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "onclick"},
              ?assertEqual([<<"CreateNewDoc()">>], ej:get(Path2, Menu)),
              PathNoneMatched = {"menu", "popup", "menuitem", {select, {"value", "NotThere"}}},
              ?assertEqual([], ej:get(PathNoneMatched, Menu)),
              PathDoesntExist = {"menu", "popup", "menuitem", {select, {"value", "NotThere"}}, "bar"},
              ?assertEqual(undefined, ej:get(PathDoesntExist, Menu)),
              Data = [
                  {struct, [{<<"match">>, <<"me">>}]},
                  {struct, [{<<"match">>, <<"me">>}]}
              ],
              ComplexBeginning = {{select, {"match", "me"}}},
              ?assertMatch([{struct, _}, {struct, _}], ej:get(ComplexBeginning, Data)),
              ComplexBeginningDeeper = {{select, {"match", "me"}}, "match"},
              ?assertMatch([<<"me">>, <<"me">>], ej:get(ComplexBeginningDeeper, Data))
            end},

          {"ej:get with multi-level array matching",
           fun() ->
                %% When doing multilevel deep array matching, we want the
                %% array returned to be a single top level list, and not
                %% a nested list of lists ...
                Data = {struct,[
                   {<<"users">>, [
                         {struct,[{<<"id">>,<<"sebastian">>},
                                  {<<"books">>, [
                                     {struct, [{<<"title">>, <<"faust">>},
                                               {<<"rating">>, 5}]}
                                  ]}
                         ]}
                   ]}
                ]},
                Path = {"users", {select, {"id", "sebastian"}}, "books",
                        {select, {"title", "faust"}}, "rating"},
                Result = ej:get(Path, Data),
                ?assertEqual([5], Result)
            end},

          {"ej:get filter at top-level",
           fun() ->
                   Data = {struct,[{<<"users">>,
                                    [{struct,[{<<"company">>,<<"opscode">>},
                                              {<<"name">>,<<"seth">>}]},
                                     {struct,[{<<"location">>,<<"Germany">>},
                                              {<<"name">>,<<"sebastian">>},
                                              {<<"company">>,<<"aircloak">>}]}]}]},
                   ?assertEqual(undefined, ej:get({"users", "company"}, Data)),
                   ?assertEqual([<<"opscode">>, <<"aircloak">>],
                                ej:get({"users", {select, all}, "company"}, Data))
           end},

          {"ej:set, replacing existing value",
           fun() ->
                   Path = {"widget", "window", "name"},
                   CurrentValue = ej:get(Path, Widget),
                   NewValue = <<"bob">>,
                   ?assert(NewValue /= CurrentValue),
                   Widget1 = ej:set(Path, Widget, NewValue),
                   ?assertEqual(NewValue, ej:get(Path, Widget1)),
                   % make sure the structure hasn't been disturbed
                   Widget2 = ej:set(Path, Widget1, <<"main_window">>),
                   ?assertEqual(Widget, Widget2)
           end},

          {"ej:set, creating new value",
           fun() ->
                   Path = {"widget", "image", "newOffset"},
                   Value = <<"YYY">>,
                   ?assertEqual(undefined, ej:get(Path, Widget)),
                   Widget1 = ej:set(Path, Widget, Value),
                   ?assertEqual(Value, ej:get(Path, Widget1))
           end},

          {"ej:set, missing intermediate path",
           fun() ->
                   Path = {"widget", "middle", "nOffset"},
                   Value = <<"YYY">>,
                   ?assertEqual(undefined, ej:get(Path, Widget)),
                   ?assertException(error, {no_path, _},
                                    ej:set(Path, Widget, Value))
           end},

          {"ej:set top-level",
           fun() ->
                   OrigVal = ej:get({"widget", "version"}, Widget),
                   NewVal = <<"2">>,
                   NewWidget = ej:set({"widget", "version"}, Widget, NewVal),
                   ?assertEqual(NewVal, ej:get({"widget", "version"}, NewWidget)),
                   Reset = ej:set({"widget", "version"}, NewWidget, OrigVal),
                   ?assertEqual(Widget, Reset)
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
                                                   "SortAs"}, Glossary1)),
                   Reset = ej:set(Path, Glossary1, <<"SGML">>),
                   ?assertEqual(Glossary, Reset)
           end},

          {"ej:set list element",
           fun() ->
                   Orig = ej:get({"menu", "popup", "menuitem", 2}, Menu),
                   New = ej:set({"onclick"}, Orig, <<"OpenFile()">>),
                   Menu1 = ej:set({"menu", "popup", "menuitem", 2}, Menu, New),
                   ?assertEqual(New,
                                ej:get({"menu", "popup", "menuitem", 2}, Menu1)),
                   Reset = ej:set({"menu", "popup", "menuitem", 2}, Menu1, Orig),
                   ?assertEqual(Menu, Reset)
           end},

          {"ej:set list element path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", 2, "onclick"},
                   Orig = ej:get(Path, Menu),
                   New = <<"OpenFile()">>,
                   Menu1 = ej:set(Path, Menu, New),
                   ?assertEqual(New, ej:get(Path, Menu1)),
                   Reset = ej:set(Path, Menu1, Orig),
                   ?assertEqual(Menu, Reset)
           end},

          {"ej:set list element path first, last",
           fun() ->
                   FPath = {"menu", "popup", "menuitem", first, "value"},
                   LPath = {"menu", "popup", "menuitem", last, "value"},
                   FMenu = ej:set(FPath, Menu, <<"create">>),
                   LMenu = ej:set(LPath, FMenu, <<"kill">>),
                   ?assertEqual(<<"create">>, ej:get(FPath, FMenu)),
                   ?assertEqual(<<"create">>, ej:get(FPath, LMenu)),
                   ?assertEqual(<<"kill">>, ej:get(LPath, LMenu))
           end},

          {"ej:set new list element",
           fun() ->
                   Path = {"menu", "popup", "menuitem", new},
                   Path1 = {"menu", "popup", "menuitem", first},
                   Menu1 = ej:set(Path, Menu, <<"first-item">>),
                   ?assertEqual(<<"first-item">>, ej:get(Path1, Menu1)),
                   List = ej:get({"menu", "popup", "menuitem"}, Menu1),
                   ?assertEqual(4, length(List))
           end},

          {"ej:set_p creates intermediate missing nodes",
           fun() ->
                   StartData = {struct,[]},
                   EndData = {struct,[{<<"a">>,
                      {struct,[{<<"b">>,
                          {struct, [{<<"c">>, <<"value">>}]}
                      }]}
                   }]},
                   Path = {"a", "b", "c"},
                   Result = ej:set_p(Path, StartData, <<"value">>),
                   ?assertEqual(EndData, Result),
                   ?assertEqual(<<"value">>, ej:get(Path, Result)),
                   Path2 = {"1", "2"},
                   Result2 = ej:set_p(Path2, Result, <<"other-value">>),
                   ?assertEqual(<<"other-value">>, ej:get(Path2, Result2)),
                   %% Does not affect existing values
                   ?assertEqual(<<"value">>, ej:get(Path, Result2))
           end},


          {"ej:set new value in an object at a complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "alt"},
                   Val = <<"helptext">>,
                   Menu1 = ej:set(Path, Menu, Val),
                   ?assertMatch([<<"helptext">>], ej:get(Path, Menu1))
           end},

          {"ej:set_p value in a non-existent object at a complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", {select, {"value", "Edit"}}},
                   Path2 = {"menu", "popup", "menuitem", {select, {"value", "Edit"}}, "text"},
                   Path3 = {"menu", "popup", "menuitem", {select, {"value", "Edit"}}, "value"},
                   Val = {struct, [{<<"text">>, <<"helptext">>}]},
                   Menu1 = ej:set_p(Path, Menu, Val),
                   ?assertMatch([<<"helptext">>], ej:get(Path2, Menu1)),
                   ?assertEqual([<<"Edit">>], ej:get(Path3, Menu1))
           end},

          {"ej:set new value in a object at a complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", {select, {"value", "New"}}},
                   Path2 = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "onclick"},
                   Val = {struct, [{<<"onclick">>, <<"CreateDifferentNewDoct()">>}]},
                   Menu1 = ej:set(Path, Menu, Val),
                   ?assertEqual([<<"CreateDifferentNewDoct()">>], ej:get(Path2, Menu1)),
                   Path3 = {"menu", "popup", "menuitem", {select,{"value", "New"}}, "speed"},
                   ValHigh = <<"high">>,
                   Menu2 = ej:set(Path3, Menu1, ValHigh),
                   ?assertEqual([ValHigh], ej:get(Path3, Menu2))
           end},

          {"ej:set replace multiple children of a complex path",
           fun() ->
                   %% We want the ability to affect multiple array elements
                   %% when a complex selector returns more than one match.
                   %% In this case all the selected array elements should be
                   %% replaced.
                   StartData = {struct, [
                      {struct, [{<<"match">>, <<"me">>}, {<<"param">>, 1}]},
                      {struct, [{<<"match">>, <<"me">>}, {<<"param">>, 2}]}
                   ]},
                   Path = {{select, {"match", "me"}}},
                   Path2 = {{select, {"match", "me"}}, "more"},
                   Val = {struct, [{<<"more">>, <<"content">>}]},
                   Result = ej:set(Path, StartData, Val),
                   ?assertMatch([<<"content">>, <<"content">>], ej:get(Path2, Result))
           end},

          {"ej:set replace multiple children deep in a complex path",
           fun() ->
                   %% We want the ability to affect multiple array elements
                   %% when a complex selector returns more than one match.
                   %% In this case we show that the array does not have to
                   %% be at the top level.
                   StartData = {struct, [{<<"parent">>, [
                          {struct, [{<<"match">>, <<"me">>}, {<<"param">>, 1}]},
                          {struct, [{<<"match">>, <<"me">>}, {<<"param">>, 2}]}
                          ]}
                   ]},
                   Path = {"parent", {select, {"match", "me"}}},
                   Path2 = {"parent", {select, {"match", "me"}}, "more"},
                   Val = {struct, [{<<"more">>, <<"content">>}]},
                   EndData = ej:set(Path, StartData, Val),
                   ?assertMatch([<<"content">>, <<"content">>], ej:get(Path2, EndData))
           end},

          {"ej:set doesn't alter order when setting a complex path",
           fun() ->
                   StartData = {struct, [{<<"parent">>, [
                          {struct, [{<<"name">>, <<"alice">>}, {<<"param">>, 1}]},
                          {struct, [{<<"name">>, <<"bob">>}, {<<"param">>, 2}]},
                          {struct, [{<<"name">>, <<"clara">>}, {<<"param">>, 3}]}
                          ]}
                   ]},
                   Path = {"parent", {select, {"name", "bob"}}, "param"},
                   EndData = ej:set(Path, StartData, 4),
                   Names = [ ej:get({"name"}, Elt) || Elt <- ej:get({"parent"}, EndData) ],
                   ExpectNames = [<<"alice">>, <<"bob">>, <<"clara">>],
                   ?assertEqual(ExpectNames, Names)
           end},

          {"ej:set should not allow replacing an array element at a complex path with a pure value",
           fun() ->
                   %% If the user has made a filtered selection on an array,
                   %% then all the elements in the array are objects.
                   %% Replacing the matched selection with a non-object value
                   %% will break this constraint.
                   Data = {struct, [{struct, [{<<"match">>, <<"me">>}]}]},
                   Path = {{select, {"match", "me"}}},
                   Val = <<"pure-value-and-not-a-struct">>,
                   ?assertException(error, {replacing_object_with_value, _},
                                      ej:set(Path, Data, Val))
           end},

          {"ej:set a value within array",
           fun() ->
                   %% We should be able to set values on elements we
                   %% have filtered out of an array, rather than just
                   %% replacing them.
                   StartData = {struct,[
                      {<<"users">>, [
                            {struct,[{<<"id">>,<<"sebastian">>}]}
                      ]}
                   ]},
                   EndData = {struct,[
                      {<<"users">>, [
                            {struct,[{<<"id">>,<<"sebastian">>},
                                     {<<"books">>, []}
                            ]}
                      ]}
                   ]},
                   Path = {"users", {select, {"id", "sebastian"}}, "books"},
                   Val = [],
                   Result = ej:set(Path, StartData, Val),
                   ?assertEqual(EndData, Result)
           end},

          {"ej:set should throw error for trying to missing intermediate nodes",
           fun() ->
                   %% If we request a composite path that doesn't exist,
                   %% and we are using set, rather than set_p, then we
                   %% should get an error thrown at us.
                   Path = {{select, {"id", "seb"}}},
                   Val = {struct, [{<<"continent">>, <<"europe">>}]},
                   ?assertException(error, {no_path, _},
                                    ej:set(Path, {struct, []}, Val))
           end},
          {"ej:set_p should construct intermediate nodes if missing",
           fun() ->
                   %% If we request a composite path that doesn't exist,
                   %% the missing nodes should be created for us dynamically
                   %% to match the filtering criteria we are searching for.
                   StartData = {struct,[]},
                   Path = {"users", {select, {"id", "seb"}}, "room",
                           {select, {"room_id", "living-room"}},
                           "books", {select, {"title", "faust"}}, "rating"},
                   Val = 5,
                   Result = ej:set_p(Path, StartData, Val),
                   ?assertEqual([5], ej:get(Path, Result))
           end},

          {"ej:set_p should create intermediate nodes if missing in existing structures",
           fun() ->
                   %% If we request a composite path that doesn't exist,
                   %% the missing nodes should be created for us dynamically
                   %% to match the filtering criteria we are searching for.
                   %% Furthermore, this should not affect old values already existing in the 
                   %% same structure.
                   StartData = {struct,[{<<"users">>,[
                        {struct,[{<<"rooms">>,[
                                    {struct,[{<<"books">>,[
                                                {struct,[{<<"rating">>,5},{<<"title">>,<<"faust">>}]}
                                          ]},{<<"room_id">>,<<"livingroom">>}
                                    ]}
                              ]},{<<"id">>,<<"seb">>}]
                        }]
                   }]},
                   ValidPath = {"users", {select, {"id", "seb"}}, 
                                "rooms", {select, {"room_id", "livingroom"}}, 
                                "books", {select, {"title", "faust"}}, "rating"},
                   ?assertEqual([5], ej:get(ValidPath, StartData)),
                   NewPath = {"users", {select, {"id", "seb"}}, 
                              "rooms", {select, {"room_id", "bathroom"}}, 
                              "sink"},
                   NewValue = true,
                   Result = ej:set_p(NewPath, StartData, NewValue),
                   ?assertEqual([true], ej:get(NewPath, Result)),
                   OtherPath = {"users", {select, {"id", "seb"}},
                                "computers", {select, {"laptop", true}}, "name"},
                   OtherValue = <<"paris">>,
                   Result1 = ej:set_p(OtherPath, Result, OtherValue),
                   io:format("~p", [Result1]),
                   ?assertEqual([<<"paris">>], ej:get(OtherPath, Result1)),
                   %% Old values still valid
                   ?assertEqual([5], ej:get(ValidPath, Result1)),
                   ?assertEqual([true], ej:get(NewPath, Result1))
           end},

          {"ej:remove",
           fun() ->
                   Path = {"glossary", "GlossDiv", "GlossList", "GlossEntry", "Abbrev"},
                   Orig = ej:get(Path, Glossary),
                   ?assert(undefined /= Orig),
                   Glossary1 = ej:delete(Path, Glossary),
                   ?assertEqual(undefined, ej:get(Path, Glossary1)),
                   % verify some structure
                   ?assertEqual(<<"SGML">>, ej:get({"glossary", "GlossDiv",
                                                    "GlossList", "GlossEntry",
                                                    "Acronym"}, Glossary1)),
                   ?assertEqual(<<"S">>, ej:get({"glossary", "GlossDiv",
                                                 "title"}, Glossary1))
           end},

          {"ej:remove parameter at complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "onclick"},
                   Orig = ej:get(Path, Menu),
                   ?assert(undefined /= Orig),
                   Menu1 = ej:delete(Path, Menu),
                   ?assertEqual([undefined], ej:get(Path, Menu1)),
                   % verify some structure
                   VerifyPath = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "value"},
                   ?assertEqual([<<"New">>], ej:get(VerifyPath, Menu1)),
                   % verify that we didn't delete siblings
                   VerifyOpen = {"menu", "popup", "menuitem", {select, {"value", "Open"}}, "onclick"},
                   ?assertEqual([<<"OpenDoc()">>], ej:get(VerifyOpen, Menu1)),
                   VerifyClose = {"menu", "popup", "menuitem", {select, {"value", "Close"}}, "onclick"},
                   ?assertEqual([<<"CloseDoc()">>], ej:get(VerifyClose, Menu1))
           end},

          {"ej:remove object at complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", {select, {"value", "New"}}},
                   Orig = ej:get(Path, Menu),
                   ?assert([] /= Orig),
                   Menu1 = ej:delete(Path, Menu),
                   ?assertEqual([], ej:get(Path, Menu1)),
                   % verify some structure
                   VerifyPath = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "value"},
                   ?assertEqual(undefined, ej:get(VerifyPath, Menu1)),
                   % % verify that we didn't delete siblings
                   VerifyOpen = {"menu", "popup", "menuitem", {select, {"value", "Open"}}, "onclick"},
                   ?assertEqual([<<"OpenDoc()">>], ej:get(VerifyOpen, Menu1)),
                   VerifyClose = {"menu", "popup", "menuitem", {select, {"value", "Close"}}, "onclick"},
                   ?assertEqual([<<"CloseDoc()">>], ej:get(VerifyClose, Menu1))
           end}
         ]
 end
}.

-endif.
