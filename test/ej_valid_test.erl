-module(ej_valid_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ej/include/ej.hrl").


basic_spec_0_test_() ->
    %% Test required key with string_match and optional key whose
    %% value must be a string.
    Spec = {[{<<"name">>, {string_match, regex_for(basic_name)}},
             {{opt, <<"description">>}, string}
            ]},
    {_, BasicRegexMsg} = regex_for(basic),
    [?_assertEqual(ok, ej:valid(Spec, basic(<<"fred">>))),

     ?_assertMatch(#ej_invalid{type = missing, key = <<"name">>},
                   ej:valid(Spec, {[]})),

     ?_assertMatch(#ej_invalid{type = string_match,
                               key = <<"name">>,
                               found = <<"&2%!">>,
                               found_type = string,
                               expected_type = string,
                               msg = BasicRegexMsg},
                   ej:valid(Spec, basic(<<"&2%!">>))),

     ?_assertEqual(ok,
                   ej:valid(Spec,
                              basic_with(<<"fred">>,
                                        [{<<"description">>, <<"blah">>}]))),

     ?_assertEqual(#ej_invalid{type = json_type,
                               expected_type = string,
                               found_type = object,
                               found = {[]},
                               key = <<"description">>},
                   ej:valid(Spec,
                              basic_with(<<"fred">>,
                                        [{<<"description">>, {[]}}]))),

     ?_assertMatch(#ej_invalid{}, ej:valid({[{<<"a">>, 12}]}, basic(<<"a">>)))
    ].

non_hash_fails_with(Val, ActualType) ->
    ?_assertEqual(#ej_invalid{type = json_type,
                              expected_type = object,
                              found_type = ActualType,
                              found = Val}, ej:valid({[]}, Val)).

non_hash_fails_test_() ->
    Tests = [
        {<<"str">>, string},
        {10, number},
        {true, boolean},
        {null, null},
        {[], array}
    ],
    [ non_hash_fails_with(Val, ActualType) || {Val, ActualType} <- Tests ].

array_map_test_() ->
    Spec = {[{<<"name">>, {string_match, regex_for(basic_name)}},
             {{opt, <<"description">>}, string},
             {<<"items">>, {array_map, {string_match, regex_for(item)}}}
            ]},
    BadItemList = basic_with(<<"fred">>,
                            [{<<"items">>, <<"abc">>}]),
    BadItemsType = basic_with(<<"fred">>,
                              [{<<"items">>, [1, 2, 3]}]),
    [
     ?_assertMatch(#ej_invalid{type = missing, key = <<"items">>},
                   ej:valid(Spec, basic(<<"fred">>))),

     ?_assertEqual(#ej_invalid{type = json_type,
                               expected_type = array,
                               found_type = string,
                               found = <<"abc">>,
                               key = <<"items">>},
                   ej:valid(Spec, BadItemList)),

     ?_assertMatch(#ej_invalid{type = array_elt,
                               expected_type = string,
                               found_type = number,
                               key = <<"items">>},
                   ej:valid(Spec, BadItemsType))
    ].

json_type_test_() ->
    Types = [string, number, null, boolean, array, object],
    SpecForType = fun(Type) ->
                          {[{<<"data">>, Type}]}
                  end,
    DataForType = fun(string) ->
                          <<"abc">>;
                     (number) ->
                             123;
                     (null) ->
                          null;
                     (boolean) ->
                          true;
                     (array) ->
                          [1.0];
                     (object) ->
                          {[]}
                  end,
    GoodForType = fun(Type) ->
                          {[{<<"data">>, DataForType(Type)}]}
                  end,
    BadForType = fun(Type) ->
                         [ {T, GoodForType(T)} || T <- (Types -- [Type]) ]
                 end,

    OkTests = [ {atom_to_list(T) ++ " ok",
                 fun() ->
                         ?assertEqual(ok, ej:valid(SpecForType(T), GoodForType(T)))
                 end} || T <- Types ],

    MissingTests = [ {atom_to_list(T) ++ " missing",
                      fun() ->
                              ?assertEqual(#ej_invalid{type = missing, key = <<"data">>,
                                                       expected_type = T},
                                           ej:valid(SpecForType(T), {[]}))
                      end} || T <- Types ],

    BadTests = [ [ {atom_to_list(T) ++ " wrong type",
                  fun() ->
                          ?assertEqual(#ej_invalid{type = json_type, key = <<"data">>,
                                                   expected_type = T,
                                                   found_type = FT,
                                                   found = DataForType(FT)},
                                       ej:valid(SpecForType(T), Bad))
                  end} || {FT, Bad} <- BadForType(T) ] || T <- Types ],
    OkTests ++ MissingTests ++ BadTests.

any_of_test_() ->
    Spec = {[ {<<"blah">>, {any_of, {[<<"foo">>, <<"other_foo">>], <<"Value must be 'foo' or 'other_foo'">>}}} ]},
    DifferentTypesSpec = {[ {<<"blah">>, {any_of, {[number, string], <<"Value must be a number or string">>}}} ]},
    EmptyAnyOfSpec = {[ {<<"blah">>, {any_of, {[], <<"No possible value could match.">>}}} ]},
    [
     ?_assertEqual(ok, ej:valid(Spec, {[{<<"blah">>, <<"foo">>}]})),
     ?_assertEqual(ok, ej:valid(Spec, {[{<<"blah">>, <<"other_foo">>}]})),
     ?_assertEqual(#ej_invalid{type = any_of,
                              key = <<"blah">>,
                              expected_type = any_value,
                              found_type = string,
                              found = <<"bjork">>,
                              msg = <<"Value must be 'foo' or 'other_foo'">>},
                   ej:valid(Spec, {[{<<"blah">>, <<"bjork">>}]})),
     ?_assertEqual(#ej_invalid{type = any_of,
                              key = <<"blah">>,
                              expected_type = any_value,
                              found_type = string,
                              found = <<"bjork">>,
                              msg = <<"No possible value could match.">>},
                   ej:valid(EmptyAnyOfSpec, {[{<<"blah">>, <<"bjork">>}]})),
     ?_assertEqual(#ej_invalid{type = missing, key = <<"blah">>, expected_type = string},
                   ej:valid(Spec, {[{}]})),
     ?_assertEqual(#ej_invalid{type = missing, key = <<"blah">>, expected_type = any_value},
                   ej:valid(DifferentTypesSpec, {[{}]})),
     ?_assertEqual(#ej_invalid{type = missing, key = <<"blah">>, expected_type = any_value},
                   ej:valid(EmptyAnyOfSpec, {[{}]}))
    ].

any_value_test_() ->
    Spec = {[ {<<"blah">>, any_value} ]},
    [
     ?_assertEqual(ok, ej:valid(Spec, {[{<<"blah">>, <<"foo">>}]})),
     ?_assertEqual(ok, ej:valid(Spec, {[{<<"blah">>, 200}]})),
     ?_assertEqual(ok, ej:valid(Spec, {[{<<"blah">>, true}]})),
     ?_assertEqual(ok, ej:valid(Spec, {[{<<"blah">>, null}]})),
     ?_assertEqual(ok, ej:valid(Spec, {[{<<"blah">>, [1,2,3]}]})),
     ?_assertEqual(ok, ej:valid(Spec, {[{<<"blah">>, [{"x",1}]}]})),
     ?_assertEqual(#ej_invalid{type = missing, key = <<"blah">>, expected_type = any_value},
                   ej:valid(Spec, {[{}]}))
    ].

literal_key_and_value_test_() ->
    Spec = {[
             {<<"class">>, <<"Memo">>}
            ]},
    [
     ?_assertEqual(ok, ej:valid(Spec, {[{<<"class">>, <<"Memo">>}]})),

     ?_assertEqual(#ej_invalid{type = exact,
                               key = <<"class">>,
                               expected_type = string,
                               found_type = string,
                               found = <<"Blah">>,
                               msg = <<"Memo">>},
                   ej:valid(Spec, {[{<<"class">>, <<"Blah">>}]})),

     ?_assertEqual(#ej_invalid{type = missing, key = <<"class">>, expected_type = string},
                   ej:valid(Spec, {[]}))
    ].

fun_match_test_() ->
    MyFun = fun(<<"aa", _V/binary>>) ->
                    ok;
               (_) ->
                    error
            end,
    Spec = {[
             {<<"akey">>, {fun_match, {MyFun, string, <<"abc">>}}}
            ]},

    Good = {[{<<"akey">>, <<"aabcdef">>}]},
    Bad = {[{<<"akey">>, <<"abcdef">>}]},
    Missing = {[]},
    BadType = {[{<<"akey">>, 123}]},
    [
     ?_assertEqual(ok, ej:valid(Spec, Good)),

     ?_assertEqual(#ej_invalid{type = fun_match, key = <<"akey">>,
                               expected_type = string,
                               found_type = string,
                               found = <<"abcdef">>,
                               msg = <<"abc">>},
                   ej:valid(Spec, Bad)),

     ?_assertEqual(#ej_invalid{type = missing, key = <<"akey">>,
                               expected_type = string},
                   ej:valid(Spec, Missing)),

     ?_assertEqual(#ej_invalid{type = json_type, key = <<"akey">>,
                              expected_type = string,
                              found_type = number,
                              found = 123},
                  ej:valid(Spec, BadType))
    ].

object_map_test_() ->
    Spec = {[
             {<<"object">>,
              {object_map, {
                 {keys, {string_match, regex_for(key)}},
                 {values, {string_match, regex_for(value)}}}}}
            ]},

    Good = {[{<<"object">>,
              {[
                {<<"k1">>, <<"v1">>},
                {<<"k2">>, <<"v2">>},
                {<<"k3">>, <<"v3">>}
               ]}}
            ]},

    GoodEmpty = {[{<<"object">>,
                   {[]}}
                 ]},

    BadKey = {[
               {<<"object">>,
                {[
                  {<<"k1">>, <<"v1">>},
                  {<<"___">>, <<"v2">>},
                  {<<"k3">>, <<"v3">>}
                 ]}}
            ]},

    BadValue = {[
                 {<<"object">>,
                  {[
                    {<<"k1">>, <<"v1">>},
                    {<<"k2">>, <<"v2">>},
                    {<<"k3">>, <<"___">>}
                   ]}}
                ]},

    BadMissing = {[]},

    BadNotObject = {[{<<"object">>, <<"notobject">>}]},

    [
     ?_assertEqual(ok, ej:valid(Spec, Good)),
     ?_assertEqual(ok, ej:valid(Spec, GoodEmpty)),

     ?_assertEqual(#ej_invalid{type = object_key, key = <<"object">>,
                               expected_type = string,
                               found_type = string,
                               found = <<"___">>,
                               msg = <<"^[[:alpha:][:digit:]]+$">>},
                   ej:valid(Spec, BadKey)),

     ?_assertEqual(#ej_invalid{type = object_value, key = <<"object">>,
                               expected_type = string,
                               found_type = string,
                               found = <<"___">>,
                               msg = <<"^[[:alpha:][:digit:]]+$">>},
                   ej:valid(Spec, BadValue)),

     ?_assertEqual(#ej_invalid{type = missing, key = <<"object">>,
                               expected_type = object},
                   ej:valid(Spec, BadMissing)),

     ?_assertEqual(#ej_invalid{type = json_type, key = <<"object">>,
                               expected_type = object,
                               found_type = string,
                               found = <<"notobject">>},
                   ej:valid(Spec, BadNotObject))
    ].

empty_top_level_object_test_() ->
    Spec = empty_object,
    GoodEmpty = {[]},
    BadNotEmpty = {[{<<"k1">>, <<"v1">>},
                    {<<"k2">>, <<"v2">>},
                    {<<"k3">>, <<"v3">>}
                   ]},
    [
     ?_assertEqual(ok, ej:valid(Spec, GoodEmpty)),
     ?_assertEqual(#ej_invalid{type = empty_object, key = undefined,
                               expected_type = object,
                               found_type = object,
                               found = BadNotEmpty},
                   ej:valid(Spec, BadNotEmpty))

    ].

empty_top_level_array_test_() ->
    Spec = empty_array,
    GoodEmpty = [],
    BadNotEmpty = [<<"a1">>, <<"a2">>, <<"a3">>],
    [
     ?_assertEqual(ok, ej:valid(Spec, GoodEmpty)),
     ?_assertEqual(#ej_invalid{type = empty_array, key = undefined,
                               expected_type = array,
                               found_type = array,
                               found = BadNotEmpty},
                   ej:valid(Spec, BadNotEmpty))
    ].

empty_object_test_() ->
    Spec = {[{<<"object">>, empty_object}]},
    GoodEmpty = {[{<<"object">>, {[]} }]},
    BadNotEmpty = {[{<<"object">>,
              {[
                {<<"k1">>, <<"v1">>},
                {<<"k2">>, <<"v2">>},
                {<<"k3">>, <<"v3">>}
               ]}}
            ]},
    BadType = {[{<<"object">>, <<"foo">>}]},

    [
     ?_assertEqual(ok, ej:valid(Spec, GoodEmpty)),
     ?_assertEqual(#ej_invalid{type = empty_object, key = <<"object">>,
                               expected_type = object,
                               found_type = object,
                               found = {[{<<"k1">>, <<"v1">>},
                                         {<<"k2">>, <<"v2">>},
                                         {<<"k3">>, <<"v3">>}
                                        ]}
                              },
                   ej:valid(Spec, BadNotEmpty)),
        ?_assertEqual(#ej_invalid{type = empty_object, key = <<"object">>,
                                  expected_type = object,
                                  found_type = string,
                                  found = <<"foo">>},
                      ej:valid(Spec, BadType))
    ].

empty_array_test_() ->
    Spec = {[{<<"array">>, empty_array}]},
    GoodEmpty = {[{<<"array">>, [] }]},
    BadNotEmpty = {[{<<"array">>, [<<"a1">>, <<"a2">>, <<"a3">>]}]},
    BadType = {[{<<"array">>, <<"foo">>}]},

    [
     ?_assertEqual(ok, ej:valid(Spec, GoodEmpty)),
     ?_assertEqual(#ej_invalid{type = empty_array, key = <<"array">>,
                               expected_type = array,
                               found_type = array,
                               found = [<<"a1">>, <<"a2">>, <<"a3">>]},
                   ej:valid(Spec, BadNotEmpty)),
        ?_assertEqual(#ej_invalid{type = empty_array, key = <<"array">>,
                                  expected_type = array,
                                  found_type = string,
                                  found = <<"foo">>},
                      ej:valid(Spec, BadType))
    ].


nested_specs_test_() ->
    Spec = {[{<<"name">>, {string_match, regex_for(name)}},
             {<<"a">>, {[
                         {<<"a_name">>, {string_match, regex_for(name)}},
                         {<<"b">>,
                          {[{<<"b_name">>, {string_match, regex_for(name)}}]}}
                        ]}}
            ]},
    {_, RegexMsg} = regex_for(name),
    Tests = [

             %% {input,
             %%  expected}

             {{[{<<"name">>, <<"top">>}]},
              #ej_invalid{type = missing, key = <<"a">>,
                          expected_type = object}},

             {{[{<<"name">>, <<"top">>}, {<<"a">>, {[]}}]},
              #ej_invalid{type = missing, key = <<"a.a_name">>, expected_type = string}},

             {{[{<<"name">>, <<"top">>},
                {<<"a">>,
                 {[{<<"a_name">>, <<"alice">>}]}}
               ]},
              #ej_invalid{type = missing, key = <<"a.b">>, expected_type = object}},

             {{[{<<"name">>, <<"top">>},
                {<<"a">>,
                 {[{<<"a_name">>, <<"alice">>},
                   {<<"b">>, {[]}}]}}
               ]},
              #ej_invalid{type = missing, key = <<"a.b.b_name">>,
                          expected_type = string}},

             {{[{<<"name">>, <<"top">>},
                {<<"a">>,
                 {[{<<"a_name">>, <<"alice">>},
                   {<<"b">>, <<"BAD">>}]}}
               ]},
              #ej_invalid{type = json_type, key = <<"a.b">>,
                          found = <<"BAD">>, found_type = string,
                          expected_type = object}},

             {{[{<<"name">>, <<"top">>},
                {<<"a">>,
                 {[{<<"a_name">>, <<"alice">>},
                   {<<"b">>,
                    {[{<<"b_name">>, <<"___">>}]}}]}}
               ]},
              #ej_invalid{type = string_match, key = <<"a.b.b_name">>,
                          found = <<"___">>,
                          found_type = string,
                          expected_type = string,
                          msg = RegexMsg}},

             {{[{<<"name">>, <<"top">>},
                {<<"a">>,
                 {[{<<"a_name">>, <<"alice">>},
                   {<<"b">>,
                    {[{<<"b_name">>, <<"bob">>}]}}]}}
               ]},
              ok}
            ],
    [ ?_assertEqual(Expect, ej:valid(Spec, In)) || {In, Expect} <- Tests ].

value_spec_as_spec_test_() ->
    Spec = {object_map, {{keys, string},
                         {values, {array_map, string}}}},
    Good = {[{<<"a">>, [<<"b">>]}]},
    Empty = {[]},
    EmptyArray = {[{<<"a">>, []}]},
    NotArray = {[{<<"a">>, <<"bad">>}]},
    WrongArrayType = {[{<<"a">>, [1, 2]}]},
    [
     ?_assertEqual(ok, ej:valid(Spec, Good)),
     ?_assertEqual(ok, ej:valid(Spec, Empty)),
     ?_assertEqual(ok, ej:valid(Spec, EmptyArray)),
     ?_assertMatch(#ej_invalid{}, ej:valid(Spec, NotArray)),
     ?_assertMatch(#ej_invalid{}, ej:valid(Spec, WrongArrayType))
    ].

struct_object_map_test_() ->
    Spec = {object_map, {{keys, string},
                         {values, {array_map, string}}}},
    Good = {struct, [{<<"a">>, [<<"b">>]}]},
    Empty = {struct, []},
    EmptyArray = {struct, [{<<"a">>, []}]},
    NotArray = {struct, [{<<"a">>, <<"bad">>}]},
    WrongArrayType = {struct, [{<<"a">>, [1, 2]}]},
    [
     ?_assertEqual(ok, ej:valid(Spec, Good)),
     ?_assertEqual(ok, ej:valid(Spec, Empty)),
     ?_assertEqual(ok, ej:valid(Spec, EmptyArray)),
     ?_assertMatch(#ej_invalid{}, ej:valid(Spec, NotArray)),
     ?_assertMatch(#ej_invalid{}, ej:valid(Spec, WrongArrayType))
    ].

basic_struct_object_test_() ->
    Spec = {[{<<"key1">>, string}]},
    Obj1 = {struct, [{<<"key1">>, <<"value1">>}]},
    Obj2 = {struct, [{<<"key1">>, {struct, [{<<"b">>, <<"value1">>}]}}]},
    [
     ?_assertEqual(ok, ej:valid(Spec, Obj1)),
     ?_assertMatch(#ej_invalid{}, ej:valid(Spec, Obj2)),
     ?_assertMatch(ok, ej:valid({[{<<"key1">>, object}]}, Obj2))
    ].

deep_struct_object_test_() ->
    A3 = {[{<<"a3">>, string}]},
    A2 = {[{<<"a2">>, A3}]},
    Spec = {[{<<"a1">>, A2}]},

    ObjOk = {struct,
             [{<<"a1">>,
               {struct, [{<<"a2">>,
                          {struct, [{<<"a3">>, <<"v">>}]}}]}
              }]},
    ObjNotOk = {struct,
                [{<<"a1">>,
                  {struct, [{<<"a2">>,
                             {struct, [{<<"a3">>, null}]}}]}
                 }]},
    [
     ?_assertEqual(ok, ej:valid(Spec, ObjOk)),
     ?_assertMatch(#ej_invalid{}, ej:valid(Spec, ObjNotOk))
    ].

basic(Name) ->
    {[{<<"name">>, Name}]}.

basic_with(Name, With) ->
    lists:foldl(fun({K, V}, Acc) ->
                        ej:set({K}, Acc, V)
                end, basic(Name), With).


regex_for(key) ->
    Pat = <<"^[[:alpha:][:digit:]]+$">>,
    {ok, Regex} = re:compile(Pat),
    {Regex, Pat};
regex_for(value) ->
    Pat = <<"^[[:alpha:][:digit:]]+$">>,
    {ok, Regex} = re:compile(Pat),
    {Regex, Pat};
regex_for(_) ->
    Pat = <<"^[[:alpha:]]+$">>,
    {ok, Regex} = re:compile(Pat),
    {Regex, Pat}.

