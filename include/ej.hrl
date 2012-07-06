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
-type json_string()  :: binary().
-type json_null()    :: null.
-type json_number()  :: integer() | float().
-type json_boolean() :: 'true' | 'false'.
-type json_array()   :: [json_term()].
-type json_plist()   :: [{json_string(), json_term()}].
-type json_object()  :: {struct, json_plist()} | {json_plist()}. % mochijson or ejson
-type json_term()    :: json_string() | json_number() | json_array() |
                        json_object() | json_null() | json_boolean().

-type key_type()    :: binary() | integer() | first | last | new.
%% TODO: Tuple of arbitrary length can't be typed; consider list which
%% can. Downside of list is confusion with strings.
-type key_tuple()   :: tuple(). %{key_type()}.

-type ej_spec_type() :: any_of        |
                        any_type      |
                        array_elt     |
                        exact         |
                        fun_match     |
                        json_type     |
                        missing       |
                        object_key    |
                        object_value  |
                        string_match.

-type ej_json_type_name() :: array    |
                             boolean  |
                             null     |
                             number   |
                             object   |
                             string.

%% The return record for validity checks
-record(ej_invalid, {
          type          :: ej_spec_type(),
          key           :: binary(),
          found         :: json_term()          | undefined,
          found_type    :: ej_json_type_name()  | any_value | undefined,
          expected_type :: ej_json_type_name()  | any_value | undefined,
          %% defined by user in spec
          msg           :: term() | undefined
       }).
