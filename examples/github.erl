-module(github).

-export([init/0]).

init() ->
    Mods = ramler:gen_client(<<"github">>, "./examples/github.raml"),
    lists:foreach(fun(AST) ->
                          {ok, Module, Bin} = compile:forms(AST),
                          code:load_binary(Module, [], Bin)
                  end, Mods).
