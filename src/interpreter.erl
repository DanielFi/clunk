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
interpret({float, _, Value}, Scope) ->
    {Value, Scope};
interpret({atom, _, Value}, Scope) ->
    {Value, Scope};
interpret({tuple, Elements}, Scope) ->
    {ReversedValues, NewScope} = lists:foldl(fun(E, {Vs, S}) -> 
        {V, NewS} = interpret(E, S),
        {[V | Vs], NewS}
    end, {[], Scope}, Elements),
    {{tuple, lists:reverse(ReversedValues)}, NewScope};
interpret({list, Elements}, Scope) ->
    {ReversedValues, NewScope} = lists:foldl(fun(E, {Vs, S}) -> 
        {V, NewS} = interpret(E, S),
        {[V | Vs], NewS}
    end, {[], Scope}, Elements),
    {{list, lists:reverse(ReversedValues)}, NewScope};
interpret({name, _, Name}, Scope) ->
    case get(Name, Scope) of
        {ok, Value} ->
            {Value, Scope};
        {error, not_found} ->
            {{name, Name}, Scope}
    end;
interpret({function, Arguments, Body}, Scope) ->
    {ReversedArgumentValues, NewScope} = lists:foldl(fun(A, {Vs, S}) ->
        {V, NewS} = interpret(A, S),
        {[V | Vs], NewS}
    end, {[], Scope}, Arguments),
    {{function, lists:reverse(ReversedArgumentValues), Body, NewScope}, NewScope};

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
    {RightValue, Scope2} = interpret(Right, Scope1),
    match(LeftValue, RightValue, Scope2);

interpret({call, Function, Parameters}, Scope) ->
    {ReversedParameterValues, Scope1} = lists:foldl(fun(P, {PVs, S}) ->
        {PV, NewS} = interpret(P, S),
        {[PV | PVs], NewS}
    end, {[], Scope}, Parameters),
    ParameterValues = lists:reverse(ReversedParameterValues),
    {{function, Arguments, Body, Closure}, Scope2} = interpret(Function, Scope1),
    InnerScope = lists:foldl(fun({A, P}, S) ->
        {_, NewS} = match(A, P, S),
        NewS
    end, #scope{parent_scope = Closure}, lists:zip(Arguments, ParameterValues)),
    {Result, _} = interpret(Body, InnerScope),
    {Result, Scope2}.

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

match(Left, {name, RName}, Scope) ->
    case get(RName, Scope) of
        {ok, RValue} ->
            match(Left, RValue, Scope);
        {error, not_found} ->
            throw(io_lib:format("name '~p' is not bound", [RName]))
    end;
match({name, LName}, Right, #scope{bound_names = BoundNames} = Scope) ->
    case get(LName, Scope) of
        {ok, LValue} ->
            match(LValue, Right, Scope);
        {error, not_found} ->
            {Right, Scope#scope{bound_names = BoundNames#{LName => Right}}}
    end;
match(Left, Right, Scope) when Left == Right andalso not is_tuple(Left)->
    {Left, Scope};
match({tuple, LValues}, {tuple, RValues}, Scope) when length(LValues) == length(RValues) ->
    {ReversedValues, NewScope} = lists:foldl(fun({L, R}, {V, S}) -> 
        {NewV, NewS} = match(L, R, S), {[NewV | V], NewS} 
    end, {[], Scope}, lists:zip(LValues, RValues)),
    {{tuple, lists:reverse(ReversedValues)}, NewScope};
match({list, LValues}, {list, RValues}, Scope) when length(LValues) == length(RValues) ->
    {ReversedValues, NewScope} = lists:foldl(fun({L, R}, {V, S}) -> 
        {NewV, NewS} = match(L, R, S), {[NewV | V], NewS} 
    end, {[], Scope}, lists:zip(LValues, RValues)),
    {{list, lists:reverse(ReversedValues)}, NewScope};
match(Left, Right, _) -> 
    throw(lists:flatten(io_lib:format("'~p' and '~p' do not match", [Left, Right]))).
