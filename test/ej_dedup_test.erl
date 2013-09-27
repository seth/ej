-module(ej_dedup_test).

-include_lib("eunit/include/eunit.hrl").

make_key(I) ->
    iolist_to_binary(["key-", integer_to_list(I)]).

%%  example data for tests

object_1(with_dups) ->
    {[
      {<<"k1">>, <<"k1">>},
      {<<"k2">>, <<"k2">>},
      {<<"k3">>, <<"k3">>},

      {<<"k1">>, <<"d1">>},
      {<<"k2">>, <<"d2">>},
      {<<"k3">>, <<"d3">>}
     ]};
object_1(no_dups) ->
    {[
      {<<"k1">>, <<"k1">>},
      {<<"k2">>, <<"k2">>},
      {<<"k3">>, <<"k3">>}
     ]}.

deep_object(with_dups) ->
    {[
      {<<"a">>, object_1(with_dups)},
      {<<"b">>, 123},
      {<<"c">>, [<<"abc">>, object_1(with_dups)]},
      {<<"d">>, {[
                  {<<"o-o-l">>, object_1(with_dups)}
                 ]}},

      {<<"a">>, <<"dup">>},
      {<<"b">>, <<"dup">>},
      {<<"c">>, [<<"dup">>, <<"dup">>]}
     ]};
deep_object(no_dups) ->
    {[
      {<<"a">>, object_1(no_dups)},
      {<<"b">>, 123},
      {<<"c">>, [<<"abc">>, object_1(no_dups)]},
      {<<"d">>, {[
                  {<<"o-o-l">>, object_1(no_dups)}
                 ]}}
     ]}.

%% same example data, but using the {struct, []} format

struct_1(with_dups) ->
    {struct,
     [
      {<<"k1">>, <<"k1">>},
      {<<"k2">>, <<"k2">>},
      {<<"k3">>, <<"k3">>},

      {<<"k1">>, <<"d1">>},
      {<<"k2">>, <<"d2">>},
      {<<"k3">>, <<"d3">>}
     ]};
struct_1(no_dups) ->
    {struct,
     [
      {<<"k1">>, <<"k1">>},
      {<<"k2">>, <<"k2">>},
      {<<"k3">>, <<"k3">>}
     ]}.

deep_struct(with_dups) ->
    {struct,
     [
      {<<"a">>, struct_1(with_dups)},
      {<<"b">>, 123},
      {<<"c">>, [<<"abc">>, struct_1(with_dups)]},
      {<<"d">>, {struct,
                 [
                  {<<"o-o-l">>, struct_1(with_dups)}
                 ]}},

      {<<"a">>, <<"dup">>},
      {<<"b">>, <<"dup">>},
      {<<"c">>, [<<"dup">>, <<"dup">>]}
     ]};
deep_struct(no_dups) ->
    {struct,
     [
      {<<"a">>, struct_1(no_dups)},
      {<<"b">>, 123},
      {<<"c">>, [<<"abc">>, struct_1(no_dups)]},
      {<<"d">>, {struct,
                 [
                  {<<"o-o-l">>, struct_1(no_dups)}
                 ]}}
     ]}.


ej_dedup_test_() ->
    [{"basic types are unchanged: no dups",
      begin
          BasicTypes0 = [
                         true, false, null, <<"a string">>, 1, 1.23,
                         {[]}, {[{<<"akey">>, <<"avalue">>}]}
                        ],
          BasicTypes1 = [BasicTypes0 | BasicTypes0],
          BasicObj = {[ {make_key(I), Val}
                        || {I, Val} <- lists:zip(lists:seq(1, length(BasicTypes1)), BasicTypes1) ]},
          BasicTypes = [BasicObj | BasicTypes1],
          [ ?_assertEqual(Elt, ej:dedup(Elt)) || Elt <- BasicTypes ]
      end},

     {"dup keys of objects are removed",
      begin
          Tests = [
                   %% flat object
                   {object_1(with_dups), object_1(no_dups)},

                   %% object w/ dups inside a list
                   {[1, object_1(with_dups), 3], [1, object_1(no_dups), 3]},

                   {deep_object(with_dups), deep_object(no_dups)},

                   %% test alternate object term syntax with 'struct'
                   {struct_1(with_dups), struct_1(no_dups)},
                   {[1, struct_1(with_dups), 3], [1, struct_1(no_dups), 3]},
                   {deep_struct(with_dups), deep_struct(no_dups)}

                  ],

          [ ?_assertEqual(Expect, ej:dedup(In)) || {In, Expect} <- Tests ]
      end}
    ].
