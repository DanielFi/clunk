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
    {ok, AST} = parser:parse(Tokens),
    
    io:format("~p", [AST]),
    io:format("~n", []),
    io:format("~p", [interpreter:interpret(AST)]),

    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
