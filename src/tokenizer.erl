-module(tokenizer).

-export([tokenize/1]).

-record(token, {type, value}). 

-define(SINGLE_CHARACTER_OPERATORS, [
    "!",
    "$",
    "%",
    "^",
    "&",
    "*",
    "(",
    ")",
    "+",
    "-",
    "/",
    "=",
    "<",
    ">",
    "[",
    "]"
    ]).

-define(TWO_CHARACTER_OPERATORS, [
    "->",
    "==",
    ">=",
    "<=",
    "!="
    ]).

%%====================================================================
%% API functions
%%====================================================================

%% Tokenize a list
tokenize(Source) when is_list(Source) -> 
    tokenize_loop("\n" ++ Source, []). 

%%====================================================================
%% Internal functions
%%====================================================================

tokenize_loop([], Tokens) -> 
    lists:reverse([#token{type = eof, value = none} | Tokens]);
tokenize_loop([_|_] = Source, Tokens) -> 
    case tokenize_any(Source) of
        {Token, Rest} -> 
            tokenize_loop(Rest, [Token | Tokens]); 
        {Rest} when is_list(Rest) ->
            tokenize_loop(Rest, Tokens);
        error ->
            io:format("Error at \"~s\"", [Source]),
            error
    end.
    
tokenize_any([D | _] = Source) when D >= $0 , $9 >= D ->
    tokenize_integer(Source);
tokenize_any([L | _] = Source) when L >= $a , $z >= L ->
    tokenize_word(Source);
tokenize_any("\n" ++ Rest) ->
    tokenize_indent(Rest);
tokenize_any([32 | Rest]) ->
    {Rest};
tokenize_any([9| Rest]) ->
    {Rest};
tokenize_any(Source) ->
    tokenize_operator(Source).

tokenize_integer(Source) ->
    {Digits, Rest} = consume_chars(Source, fun is_digit/1),
    {#token{type = integer, value = list_to_integer(Digits)}, Rest}.

tokenize_word(Source) -> 
    {Chars, Rest} = consume_chars(Source, fun is_alphanumeric/1),
    {#token{type = word, value = Chars}, Rest}.

tokenize_indent(" " ++ _ = Source) -> 
    {Spaces, Rest} = consume_chars(Source, " "),
    [H | _] = Rest, 
    case H of
        10 ->
            {Rest};
        _ ->
            {#token{type = indent, value = length(Spaces) / 4}, Rest}
    end;
tokenize_indent("\t" ++ _ = Source) ->
    {Tabs, Rest} = consume_chars(Source, "\t"),
    [H | _] = Rest, 
    case H of
        10 ->
            {Rest};
        _ ->
            {#token{type = indent, value = length(Tabs)}, Rest}
    end;
tokenize_indent(Source) ->
    {#token{type = indent, value = 0}, Source}.

tokenize_operator(Source) ->
    case tokenize_two_char_operator(Source) of
        {Token = #token{}, Rest} ->
            {Token, Rest};
        error ->
            tokenize_single_char_operator(Source)
    end.

tokenize_two_char_operator(Source) when length(Source) < 2 ->
    error;
tokenize_two_char_operator([Char1, Char2 | Rest]) ->
    TwoChars = [Char1, Char2],
    case lists:member(TwoChars, ?TWO_CHARACTER_OPERATORS) of
        false ->
            error;
        true ->
            {#token{type = operator, value = list_to_atom(TwoChars)}, Rest}
    end.

tokenize_single_char_operator([H | Rest]) ->
    case lists:member([H], ?SINGLE_CHARACTER_OPERATORS) of
        false ->
            error;
        true ->
            {#token{type = operator, value = list_to_atom([H])}, Rest}
    end.

%%====================================================================
%% Helper functions
%%====================================================================

%% Consume characters until the first unwanted character. 
%% Can be given a list of valid characters or a filter function. 
consume_chars(Source, ValidChars) when is_list(ValidChars) -> 
    consume_chars(Source, ValidChars, []);
consume_chars(Source, FilterFun) when is_function(FilterFun) ->
    consume_chars(Source, FilterFun, []).
consume_chars([H | Tail] = Source, ValidChars, Reversed) when is_list(ValidChars) ->
    case lists:member(H, ValidChars) of
        false ->
            {lists:reverse(Reversed), Source};
        true -> 
            consume_chars(Tail, ValidChars, [H | Reversed])
    end;
consume_chars([H | Tail] = Source, FilterFun, Reversed) when is_function(FilterFun) ->
    case FilterFun(H) of
        false -> 
            {lists:reverse(Reversed), Source};
        true -> 
            consume_chars(Tail, FilterFun, [H | Reversed])
    end.

consume_until_whitespace(Source) ->
    consume_until_whitespace(Source, []).
consume_until_whitespace([C | Rest], Reversed) when C == 32 ; C == 10  ; C == 9 -> 
    {lists:reverse(Reversed), Rest};
consume_until_whitespace([C | Rest], Reversed) ->
    consume_until_whitespace(Rest, [C | Reversed]).

is_digit(C) -> 
    C >= $0 andalso $9 >= C.

is_alpha(C) ->
    (C >= $a andalso $z >= C) orelse (C >= $A andalso $Z >= C).

is_alphanumeric(C) -> 
    is_digit(C) orelse is_alpha(C).
