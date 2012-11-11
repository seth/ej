-module(ej_alt_test).

-include_lib("eunit/include/eunit.hrl").

ej_alt_test_() ->
{setup,
 fun() ->
         {ok, [Widget]} = file:consult("../test/widget.alt_terms"),
         {ok, [Glossary]} = file:consult("../test/glossary.alt_terms"),
         {ok, [Menu]} = file:consult("../test/menu.alt_terms"),
         ObjList = {[{<<"objects">>,
                      [ {[{<<"id">>, I}]} ||
                          I <- lists:seq(1, 5) ]}]},
         {Widget, Glossary, Menu, ObjList}
 end,
 fun({Widget, Glossary, Menu, ObjList}) ->
         [{"ej:get",
           [
            ?_assertMatch({[{_, _}|_]}, ej:get({"widget"}, Widget)),
            ?_assertEqual(<<"1">>, ej:get({"widget", "version"}, Widget)),
            ?_assertEqual(250, ej:get({"widget", "image", "hOffset"}, Widget)),
            ?_assertEqual([1,2,3,4,5], ej:get({"widget", "values"}, Widget)),
            ?_assertEqual(2, ej:get({"widget", "values", 2}, Widget)),
            ?_assertEqual(4, ej:get({"widget", "values", 4}, Widget)),
            ?_assertEqual(1, ej:get({"widget", "values", first}, Widget)),
            ?_assertEqual(5, ej:get({"widget", "values", last}, Widget)),
            ?_assertEqual({[{<<"id">>, 5}]},
                          ej:get({<<"objects">>, last}, ObjList)),
            ?_assertEqual({[{<<"id">>, 1}]},
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

          {"ej:get from array by matching key",
           fun() ->
              Path1 = {"menu", "popup", "menuitem", {"value", "New"}},
              ?assertMatch([{[{<<"value">>,<<"New">>}|_]}], ej:get(Path1, Menu)),
              Path2 = {"menu", "popup", "menuitem", {"value", "New"}, "onclick"},
              ?assertEqual([<<"CreateNewDoc()">>], ej:get(Path2, Menu)),
              PathNoneMatched = {"menu", "popup", "menuitem", {"value", "NotThere"}},
              ?assertEqual([], ej:get(PathNoneMatched, Menu)),
              PathDoesntExist = {"menu", "popup", "menuitem", {"value", "NotThere"}, "bar"},
              ?assertEqual(undefined, ej:get(PathDoesntExist, Menu)),
              Data = {[
                       {[{<<"match">>, <<"me">>}]},
                       {[{<<"match">>, <<"me">>}]}
                      ]},
              ComplexBeginning = {{"match", "me"}},
              ?assertMatch([{_}, {_}], ej:get(ComplexBeginning, Data)),
              ComplexBeginningDeeper = {{"match", "me"}, "match"},
              ?assertMatch([<<"me">>, <<"me">>], ej:get(ComplexBeginningDeeper, Data))
            end},
          {"ej:get with multi-level array matching",
           fun() ->
                %% When doing multilevel deep array matching, we want the
                %% array returned to be a single top level list, and not
                %% a nested list of lists ...
                Data = {[
                   {<<"users">>, [
                         {[{<<"id">>,<<"sebastian">>},
                                  {<<"books">>, [
                                     {[{<<"title">>, <<"faust">>},
                                       {<<"rating">>, 5}]}
                                  ]}
                         ]}
                   ]}
                ]},
                Path = {"users", {"id", "sebastian"}, "books", {"title", "faust"}, "rating"},
                Result = ej:get(Path, Data),
                ?assertEqual([5], Result)
            end},

          {"ej:set_p creates intermediate missing nodes",
           fun() ->
                   StartData = {[]},
                   EndData = {[{<<"a">>,
                      {[{<<"b">>,
                          { [{<<"c">>, <<"value">>}]}
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
                   Path = {"menu", "popup", "menuitem", {"value", "New"}, "alt"},
                   Val = <<"helptext">>,
                   Menu1 = ej:set(Path, Menu, Val),
                   ?assertMatch([<<"helptext">>], ej:get(Path, Menu1))
           end},
          {"ej:set_p value in a non-existent object at a complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", {"value", "Edit"}},
                   Path2 = {"menu", "popup", "menuitem", {"value", "Edit"}, "text"},
                   Path3 = {"menu", "popup", "menuitem", {"value", "Edit"}, "value"},
                   Val = { [{<<"text">>, <<"helptext">>}]},
                   Menu1 = ej:set_p(Path, Menu, Val),
                   ?assertMatch([<<"helptext">>], ej:get(Path2, Menu1)),
                   ?assertEqual([<<"Edit">>], ej:get(Path3, Menu1))
           end},

          {"ej:set new value in a object at a complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", {"value", "New"}},
                   Path2 = {"menu", "popup", "menuitem", {"value", "New"}, "onclick"},
                   Val = { [{<<"onclick">>, <<"CreateDifferentNewDoct()">>}]},
                   Menu1 = ej:set(Path, Menu, Val),
                   ?assertEqual([<<"CreateDifferentNewDoct()">>], ej:get(Path2, Menu1)),
                   Path3 = {"menu", "popup", "menuitem", {"value", "New"}, "speed"},
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
                   StartData = { [
                      { [{<<"match">>, <<"me">>}, {<<"param">>, 1}]},
                      { [{<<"match">>, <<"me">>}, {<<"param">>, 2}]}
                   ]},
                   Path = {{"match", "me"}},
                   Path2 = {{"match", "me"}, "more"},
                   Val = { [{<<"more">>, <<"content">>}]},
                   Result = ej:set(Path, StartData, Val),
                   ?assertMatch([<<"content">>, <<"content">>], ej:get(Path2, Result))
           end},

          {"ej:set replace multiple children deep in a complex path",
           fun() ->
                   %% We want the ability to affect multiple array elements
                   %% when a complex selector returns more than one match.
                   %% In this case we show that the array does not have to
                   %% be at the top level.
                   StartData = { [{<<"parent">>, [
                          { [{<<"match">>, <<"me">>}, {<<"param">>, 1}]},
                          { [{<<"match">>, <<"me">>}, {<<"param">>, 2}]}
                          ]}
                   ]},
                   Path = {"parent", {"match", "me"}},
                   Path2 = {"parent", {"match", "me"}, "more"},
                   Val = { [{<<"more">>, <<"content">>}]},
                   EndData = ej:set(Path, StartData, Val),
                   ?assertMatch([<<"content">>, <<"content">>], ej:get(Path2, EndData))
           end},

          {"ej:set should not allow replacing an array element at a complex path with a pure value",
           fun() ->
                   %% If the user has made a filtered selection on an array,
                   %% then all the elements in the array are objects.
                   %% Replacing the matched selection with a non-object value
                   %% will break this constraint.
                   Data = { [{ [{<<"match">>, <<"me">>}]}]},
                   Path = {{"match", "me"}},
                   Val = <<"pure-value-and-not-a-struct">>,
                   ?assertException(error, {replacing_object_with_value, _},
                                      ej:set(Path, Data, Val))
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
           end}
         ]
 end
}.

