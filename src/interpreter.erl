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
            interpret_rhs(E, S)
        end, {nil, Scope}, Expressions);

interpret({integer, _, Value}, Scope) ->
    {Value, Scope};
interpret({name, _, Name}, Scope) ->
    {resolve({name, Name}, Scope#scope.bound_names), Scope};

interpret({{'+', _}, Left, Right}, Scope) ->
    {LeftValue, Scope1} = interpret_rhs(Left, Scope),
    {RightValue, Scope2} = interpret_rhs(Right, Scope1),
    {LeftValue + RightValue, Scope2};
interpret({{'-', _}, Left, Right}, Scope) ->
    {LeftValue, Scope1} = interpret_rhs(Left, Scope),
    {RightValue, Scope2} = interpret_rhs(Right, Scope1),
    {LeftValue - RightValue, Scope2};
interpret({{'*', _}, Left, Right}, Scope) ->
    {LeftValue, Scope1} = interpret_rhs(Left, Scope),
    {RightValue, Scope2} = interpret_rhs(Right, Scope1),
    {LeftValue * RightValue, Scope2};
interpret({{'/', _}, Left, Right}, Scope) ->
    {LeftValue, Scope1} = interpret_rhs(Left, Scope),
    {RightValue, Scope2} = interpret_rhs(Right, Scope1),
    {LeftValue / RightValue, Scope2};
interpret({{'%', _}, Left, Right}, Scope) ->
    {LeftValue, Scope1} = interpret_rhs(Left, Scope),
    {RightValue, Scope2} = interpret_rhs(Right, Scope1),
    {LeftValue rem RightValue, Scope2};
interpret({{'^', _}, Left, Right}, Scope) ->
    {LeftValue, Scope1} = interpret_rhs(Left, Scope),
    {RightValue, Scope2} = interpret_rhs(Right, Scope1),
    {math:pow(LeftValue, RightValue), Scope2};

interpret({{'=', _}, Left, Right}, Scope) ->
    {LeftValue, Scope1} = interpret(Left, Scope),
    {RightValue, #scope{bound_names = BoundNames} = Scope2} = interpret_rhs(Right, Scope1),
    case resolve(LeftValue, Scope2) of
        {name, Name} -> 
            {RightValue, Scope2#scope{bound_names = BoundNames#{Name => RightValue}}};
        _ ->
            match(LeftValue, RightValue, Scope2)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

resolve({name, Name}, #scope{parent_scope = Parent, bound_names = BoundNames}) ->
    case {maps:is_key(Name, BoundNames), Parent} of
        {true, _} ->
            maps:get(Name, BoundNames);
        {false, nil} ->
            {name, Name};
        {false, _} ->
            resolve({name, Name}, Parent)
    end;
resolve(Value, _) ->
    Value.

interpret_rhs(Node, Scope) ->
    {Value, Scope1} = interpret(Node, Scope),
    {resolve(Value, Scope1), Scope1}.

match(Left, Right, Scope) when Left == Right ->
    {Left, Scope}.
