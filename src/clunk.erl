-module(clunk).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),

    [Filename] = Args,
    {ok, BinaryContents} = file:read_file(Filename),
    io:format(binary_to_list(BinaryContents)),
    io:format("~p", [tokenizer:tokenize(binary_to_list(BinaryContents))]),

    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
