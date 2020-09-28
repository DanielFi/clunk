-module(clunk).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

main(Args) ->
    [Filename] = Args,
    {ok, BinaryContents} = file:read_file(Filename),
    {ok, Tokens, _} = lexer:string(BinaryContents),
    io:format("~p", [parser:parse(Tokens)]),

    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
