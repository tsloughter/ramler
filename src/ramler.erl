%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <t@crashfast.com>
%%% @copyright (C) 2013, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 26 Oct 2013 by Tristan Sloughter <t@crashfast.com>
%%%-------------------------------------------------------------------

-module(ramler).

-export([gen_client/2]).

-compile({parse_transform, parse_trans_codegen}).

gen_client(Prefix, Spec) ->
    {ok, [Raml]} = yaml:load_file(Spec, [implicit_atoms]),
    {Root, Endpoints} = split_options(Raml),
    BaseUri = proplists:get_value(baseUri, Root, ""),
    lists:map(fun([{<<"/", Endpoint/binary>>, Rest}]) ->
                      ModuleName = binary_to_atom(<<Prefix/binary, "_", Endpoint/binary>>, latin1),
                      {Config, SubEndpoints} = split_options(Rest),
                      FullPath = filename:join([<<"/">>, <<"">>, Endpoint]),
                      {Exports, Methods} = gen_methods(BaseUri, FullPath, Config),
                      {Exports1, Methods1} =
                          handle_sub_endpoints(BaseUri,
                                               Endpoint,
                                               SubEndpoints, Exports, Methods),
                      [{attribute, 1, module, ModuleName},
                       {attribute, 1, export, Exports1}
                       | Methods1]
               end, Endpoints).

handle_sub_endpoints(_BaseUri, _RootPath, [],
                    ExportAcc, MethodAcc) ->
    {ExportAcc, MethodAcc};
handle_sub_endpoints(BaseUri, RootPath, [[{<<"/", Endpoint/binary>>, Rest}] | T],
                    ExportAcc, MethodAcc) ->
    {Config, SubEndpoints} = split_options(Rest),
    FullPath = filename:join([<<"/">>, RootPath, Endpoint]),
    {Exports, Methods} = gen_methods(BaseUri, FullPath, Config),
    handle_sub_endpoints(BaseUri, Endpoint, SubEndpoints++T,
                         Exports++ExportAcc, Methods++MethodAcc).

gen_methods(BaseUri, Endpoint, Methods) ->
    gen_methods(BaseUri, Endpoint, Methods, {[], []}).

gen_methods(_BaseUri, _Endpoint, [], Acc) ->
    Acc;
gen_methods(BaseUri, Endpoint, [{Method, Options} | T], {ExportAcc, MethodAcc}) ->
    MethodInternal = list_to_atom(atom_to_list(Method)++"_"),
    UriParams = proplists:get_value(uriParameters, Options, []),
    QueryParams = proplists:get_value(queryParameters, Options, []),
    ExportedFuns = gen_req_fun(Method, MethodInternal, UriParams),
    Exports = [{Method, length(UriParams)}, {Method, length(UriParams)+1}],
    Funs = codegen:gen_function(MethodInternal,
                                fun(Required, Optional) ->
                                        ramler_utils:request({'$var', Method}, {'$var', BaseUri}, {'$var', Endpoint}, Required, Optional, {'$var', QueryParams})
                                end),
    gen_methods(BaseUri, Endpoint, T, {Exports++ExportAcc, [Funs | ExportedFuns]++MethodAcc}).

split_options(Raml) ->
    lists:foldl(fun({Key, _Value}=KV, {Root, Endpoints}) when is_atom(Key) ->
                        {[KV | Root], Endpoints};
                   (KV, {Root, Endpoints}) ->
                        {Root, [[KV] | Endpoints]}
                end, {[], []}, Raml).

gen_req_fun(Method, MethodInternal, Required) ->
    Vars = [{var, 1, X} || {X, _Type} <- Required],
    Cons = to_cons(Vars),
    [{function, 1, Method, length(Required),
      [
      {clause, 1,
       [{var, 1, upper_first_char(X)} || {var, _, X} <- Vars],
       [],
       [{call, 1, {atom, 1, MethodInternal}, [Cons, {nil, 1}]}]}]},
    {function, 1, Method, length(Required)+1,
     [{clause, 1,
       [{var, 1, upper_first_char(X)} || {var, _, X} <- Vars] ++ [{var, 1, 'Optional'}],
       [],
       [{call, 1, {atom, 1, MethodInternal}, [Cons, {var, 1, 'Optional'}]}]}]}].

to_cons([]) ->
    {nil, 1};
to_cons([{var, _, H}]) ->
    {cons, 1, {tuple, 1, [{bin,1,
                                 [{bin_element,1,
                                   {string,1,atom_to_list(H)},
                                   default,default}]}
                                , {var, 1, upper_first_char(H)}]}, {nil, 1}};
to_cons([{var, _, H} | T]) ->
    {cons, 1, {tuple, 1, [{bin,1,
                                 [{bin_element,1,
                                   {string,1,atom_to_list(H)},
                                   default,default}]}
                                , {var, 1, upper_first_char(H)}]}, to_cons(T)}.

to_erlang(Ast) when is_list(Ast) ->
    erl_prettypr:format(erl_syntax:form_list(Ast));
to_erlang(Ast) ->
    erl_prettypr:format(erl_syntax:form_list([Ast])).

upper_first_char(A) when is_atom(A) ->
    upper_first_char(atom_to_list(A));
upper_first_char([H | T]) ->
    list_to_atom([string:to_upper(H) | T]).
