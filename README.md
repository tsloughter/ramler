RAMLER
======

Erlang client and server generator for [RAML](http://raml.org/), the RESTful API Modeling Language.

## Build

```
$ sudo apt-get install libyaml-dev
$ make
```

## Example

```
$ erlc -o ./examples examples/github.erl
$ erl -pa ebin -pa examples -env ERL_LIBS deps
1> application:ensure_all_started(hackney).
{ok,[crypto,asn1,public_key,ssl,mimetypes,hackney]}
2> application:ensure_all_started(jsx).
{ok,[jsx]}
3> github:init().
ok
4> github_user:get().
[{<<"message">>,<<"Requires authentication">>},
 {<<"documentation_url">>,
  <<"http://developer.github.com/v3">>}]
```
