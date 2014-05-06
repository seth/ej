

# Module ej #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Tools for working with Erlang terms representing JSON.
Copyright (c) Copyright 2011-2012 Seth Falcon


__Authors:__ Seth Falcon ([`seth@userprimary.net`](mailto:seth@userprimary.net)).
<a name="description"></a>

## Description ##


The ej module is intended to make it easy to work with the Erlang
structure used by `mochijson2` to represent JSON.  You can use
`ej:get` to walk an object and return a particular value, or
`ej:set` to update a value.

<a name="types"></a>

## Data Types ##




### <a name="type-ej_array_map">ej_array_map()</a> ###



<pre><code>
ej_array_map() = {array_map, <a href="#type-ej_json_val_spec">ej_json_val_spec()</a>}
</code></pre>





### <a name="type-ej_fun_match">ej_fun_match()</a> ###



<pre><code>
ej_fun_match() = {fun_match, {fun((<a href="#type-json_term">json_term()</a>) -&gt; ok | error), <a href="#type-ej_json_type_name">ej_json_type_name()</a>, term()}}
</code></pre>





### <a name="type-ej_json_key_spec">ej_json_key_spec()</a> ###



<pre><code>
ej_json_key_spec() = binary() | {opt, binary()}
</code></pre>





### <a name="type-ej_json_spec">ej_json_spec()</a> ###



<pre><code>
ej_json_spec() = {[<a href="#type-ej_json_spec_rule">ej_json_spec_rule()</a>]} | <a href="#type-ej_object_map">ej_object_map()</a>
</code></pre>





### <a name="type-ej_json_spec_rule">ej_json_spec_rule()</a> ###



<pre><code>
ej_json_spec_rule() = {<a href="#type-ej_json_key_spec">ej_json_key_spec()</a>, <a href="#type-ej_json_val_spec">ej_json_val_spec()</a>}
</code></pre>





### <a name="type-ej_json_val_spec">ej_json_val_spec()</a> ###



<pre><code>
ej_json_val_spec() = binary() | <a href="#type-ej_json_type_name">ej_json_type_name()</a> | <a href="#type-ej_string_match">ej_string_match()</a> | <a href="#type-ej_fun_match">ej_fun_match()</a> | <a href="#type-ej_array_map">ej_array_map()</a> | <a href="#type-ej_object_map">ej_object_map()</a> | {[<a href="#type-ej_json_val_spec">ej_json_val_spec()</a>]}
</code></pre>





### <a name="type-ej_object_map">ej_object_map()</a> ###



<pre><code>
ej_object_map() = {object_map, {{keys, <a href="#type-ej_json_val_spec">ej_json_val_spec()</a>}, {values, <a href="#type-ej_json_val_spec">ej_json_val_spec()</a>}}}
</code></pre>





### <a name="type-ej_string_match">ej_string_match()</a> ###



<pre><code>
ej_string_match() = {string_match, {<a href="re.md#type-mp">re:mp()</a>, term()}}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#dedup-1">dedup/1</a></td><td>Recursively remove all but the first occurance of duplicated
keys in EJSON objects.</td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td>Remove the item specified by <code>Keys</code>.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>Extract a value from <code>Obj</code></td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td>same as get/2, but returns <code>Default</code> if the specified value was not found.</td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td>Set a value in <code>Obj</code></td></tr><tr><td valign="top"><a href="#set_p-3">set_p/3</a></td><td>Set a value in <code>Obj</code> and create missing intermediate
nodes if need be.</td></tr><tr><td valign="top"><a href="#valid-2">valid/2</a></td><td>Validate JSON terms.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="dedup-1"></a>

### dedup/1 ###

`dedup(Val) -> any()`


Recursively remove all but the first occurance of duplicated
keys in EJSON objects. According to
[`http://www.ietf.org/rfc/rfc4627.txt`](http://www.ietf.org/rfc/rfc4627.txt) JSON objects SHOULD have
unique keys, but it is not a hard requirement. Some parsers fail
when encountering duplicate keys.


The spec for `dedup` should be `-spec dedup(json_term()) ->
json_term().`, but dialyzer with `-Wunderspecs` warns on this I
think because it exceeds the depth that dialyzer can look.
<a name="delete-2"></a>

### delete/2 ###


<pre><code>
delete(Keys::<a href="#type-ej_key_path">ej_key_path()</a>, Obj::<a href="#type-json_object">json_object()</a>) -&gt; <a href="#type-json_object">json_object()</a>
</code></pre>

<br></br>


Remove the item specified by `Keys`.
<a name="get-2"></a>

### get/2 ###


<pre><code>
get(Keys::<a href="#type-ej_key_path">ej_key_path()</a>, Obj::<a href="#type-json_object">json_object()</a> | <a href="#type-json_plist">json_plist()</a>) -&gt; <a href="#type-json_term">json_term()</a> | undefined
</code></pre>

<br></br>



Extract a value from `Obj`



`Keys` is a tuple or list specifying a path into the JSON
structure.  Each string or binary element of `Keys` will act like a
Javascript property lookup.  Elements of JSON arrays can be
accessed by including an integer as an element of `Keys`.  In
addition, the atoms `'first'` and `'last'` can be used to
access the first and last elements of a list, respectively.



Additionally, a subset of JSON objects in an array can be selected
by matching on a key/value pair. This is best explained with an
example (for compactness, the input and output is shown using JSON
notation instead of EJSON).


Given:

```
  Cakes = {[
            {<<"cakes">>, [
                           {[{<<"frosting">>, <<"white">>}, {<<"tastes">>, <<"good">>}]},
                           {[{<<"frosting">>, <<"red">>},   {<<"tastes">>, <<"good">>}]},
                           {[{<<"frosting">>, <<"blue">>},  {<<"tastes">>, <<"bad">>}]}
                          ]
            }
           ]}.
```



Then you can select the good tasting cakes like this:



```
  ej:get({"cakes", {select {"tastes", "good"}}}, Cakes).
  [
   {[{<<"frosting">>, <<"white">>}, {<<"tastes">>, <<"good">>}]},
   {[{<<"frosting">>, <<"red">>},   {<<"tastes">>, <<"good">>}]}
  ]
```


<a name="get-3"></a>

### get/3 ###


<pre><code>
get(Keys::<a href="#type-ej_key_path">ej_key_path()</a>, Obj::<a href="#type-json_object">json_object()</a> | <a href="#type-json_plist">json_plist()</a>, Default::<a href="#type-json_term">json_term()</a>) -&gt; <a href="#type-json_term">json_term()</a>
</code></pre>

<br></br>


same as get/2, but returns `Default` if the specified value was not found.
<a name="set-3"></a>

### set/3 ###


<pre><code>
set(Keys::<a href="#type-ej_key_path">ej_key_path()</a>, Obj::<a href="#type-json_object">json_object()</a>, Value::<a href="#type-json_term">json_term()</a>) -&gt; <a href="#type-json_term">json_term()</a>
</code></pre>

<br></br>



Set a value in `Obj`


Replaces the value at the path specified by `Keys` with `Value` and
returns the new structure.  If `Value` is the atom `EJ_DELETE`,
then the path specified by `Keys` is removed (but see `delete/2`).

<a name="set_p-3"></a>

### set_p/3 ###


<pre><code>
set_p(Keys::<a href="#type-ej_key_path">ej_key_path()</a>, Obj::<a href="#type-json_object">json_object()</a>, Value::<a href="#type-json_term">json_term()</a>) -&gt; <a href="#type-json_term">json_term()</a>
</code></pre>

<br></br>



Set a value in `Obj` and create missing intermediate
nodes if need be.



This resembles the behavior of `mkdir -p`. If the intermediate
elements in the structure are missing, then they are created.  This
is useful when creating complex EJSON structures from scratch.



The arguments are the same as for `set`.


Example:

```
  ej:set_p({"users", {select, {"name", "sebastian"}}, "location"},
           {[]},
           <<"Germany">>).
  {[{<<"users">>,
     [{[{<<"location">>, <<"Germany">>},
        {<<"name">>, <<"sebastian">>}]}]}]}
```


<a name="valid-2"></a>

### valid/2 ###


<pre><code>
valid(Spec::<a href="#type-ej_json_spec">ej_json_spec()</a>, Obj::<a href="#type-json_object">json_object()</a>) -&gt; ok | #ej_invalid{}
</code></pre>

<br></br>


Validate JSON terms. Validity is determined by the
`ej_json_spec()` provided which has the shape of EJSON terms but
with keys and values describing what is expected. `Obj` is the
EJSON term to be validated. This function will return `ok` if all
validation rules succeed and a `#ej_invalid{}` record when the
first failure is encountered (validation specs are processed in
order, depth first).  NOTE: this function is experimental and the
API and definition of specs is subject to change.
