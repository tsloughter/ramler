-module(github).

-export([init/0]).

init() ->
    application:ensure_all_started(hackney),
    Mods = ramler:gen_client(<<"github">>, "./examples/github.raml"),
    lists:foreach(fun(AST) ->
                          {ok, Module, Bin} = compile:forms(AST),
                          code:load_binary(Module, [], Bin)
                  end, Mods).
