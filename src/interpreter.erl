-module(interpreter).

%% API exports
-export([interpret/1,interpret/2]).

-record(scope, {parent_scope = nil, bound_names = #{}}).

%%====================================================================
%% API functions
%%====================================================================

interpret(Node) -> 
    interpret(Node, #scope{}).

interpret({expressions, Expressions}, Scope) -> 
    lists:foldl(fun(E, {_, S}) -> 
            interpret(E, S)
        end, {nil, Scope}, Expressions);

interpret({integer, _, Value}, Scope) ->
    {Value, Scope};
interpret({name, _, Name}, Scope) ->
    case get(Name, Scope) of
        {ok, Value} ->
            {Value, Scope};
        {error, not_found} ->
            {{name, Name}, Scope}
    end;

interpret({{'+', _}, Left, Right}, Scope) ->
    {LeftValue, Scope1} = interpret(Left, Scope),
    {RightValue, Scope2} = interpret(Right, Scope1),
    {LeftValue + RightValue, Scope2};
interpret({{'-', _}, Left, Right}, Scope) ->
    {LeftValue, Scope1} = interpret(Left, Scope),
    {RightValue, Scope2} = interpret(Right, Scope1),
    {LeftValue - RightValue, Scope2};
interpret({{'*', _}, Left, Right}, Scope) ->
    {LeftValue, Scope1} = interpret(Left, Scope),
    {RightValue, Scope2} = interpret(Right, Scope1),
    {LeftValue * RightValue, Scope2};
interpret({{'/', _}, Left, Right}, Scope) ->
    {LeftValue, Scope1} = interpret(Left, Scope),
    {RightValue, Scope2} = interpret(Right, Scope1),
    {LeftValue / RightValue, Scope2};
interpret({{'%', _}, Left, Right}, Scope) ->
    {LeftValue, Scope1} = interpret(Left, Scope),
    {RightValue, Scope2} = interpret(Right, Scope1),
    {LeftValue rem RightValue, Scope2};
interpret({{'^', _}, Left, Right}, Scope) ->
    {LeftValue, Scope1} = interpret(Left, Scope),
    {RightValue, Scope2} = interpret(Right, Scope1),
    {math:pow(LeftValue, RightValue), Scope2};

interpret({{'=', _}, Left, Right}, Scope) ->
    {LeftValue, Scope1} = interpret(Left, Scope),
    {RightValue, #scope{bound_names = BoundNames} = Scope2} = interpret(Right, Scope1),
    case LeftValue of
        {name, Name} -> 
            {RightValue, Scope2#scope{bound_names = BoundNames#{Name => RightValue}}};
        _ ->
            match(LeftValue, RightValue, Scope2)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

get(Name, #scope{parent_scope = Parent, bound_names = BoundNames}) ->
    case {maps:is_key(Name, BoundNames), Parent} of
        {true, _} ->
            {ok, maps:get(Name, BoundNames)};
        {false, nil} ->
            {error, not_found};
        {false, _} ->
            get(Name, Parent)
    end.

match(Left, Right, Scope) when Left == Right ->
    {Left, Scope}.
