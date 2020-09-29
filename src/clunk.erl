-module(clunk).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

main(Args) ->
    [Filename] = Args,
    {ok, BinaryContents} = file:read_file(Filename),
    {ok, Tokens, _} = lexer:string(binary_to_list(BinaryContents)),
    io:format("~p~n", [Tokens]),
    {ok, AST} = parser:parse(Tokens),
    io:format("~p~n", [AST]),
    io:format("~p~n", [interpreter:interpret(AST)]),

    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
