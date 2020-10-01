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
interpret({{'<-', _}, Left, Right}, Scope) ->
    {LeftValue, Scope1} = interpret(Left, Scope),
    case interpret(Right, Scope1) of
        {{tuple, RightValues}, Scope2} ->
            LastRightValue = lists:last(RightValues),
            match(LeftValue, LastRightValue, Scope2);
        {{function, _, _, _} = Function, Scope2} ->
            match(LeftValue, last_wrap_tuple_callable(Function), Scope2)
    end;

interpret({{'->', _}, Argument, Body}, Scope) ->
    {ArgumentValue, Scope1} = interpret(Argument, Scope),
    {{function, ArgumentValue, Body, Scope1}, Scope1};

interpret({call, Function, Parameter}, Scope) ->
    {ParameterValue, Scope1} = interpret(Parameter, Scope),
    {Callable, Scope2} = interpret(Function, Scope1),
    {Result, _} = call(Callable, ParameterValue),
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

call({function, Argument, Body, Closure}, Parameter) ->
    {_, CallScope} = match(Argument, Parameter, Closure),
    interpret(Body, CallScope);
call({tuple, Callables}, Parameter) ->
    {Success, FinalResult} = lists:foldl(fun(Callable, {Matched, Result}) ->
        case Matched of
            true ->
                {true, Result};
            false ->
                try {true, call(Callable, Parameter)}
                catch
                    _ -> {false, nil}
            end
        end
    end, {false, nil}, Callables),
    case Success of
        true ->
            FinalResult;
        false ->
            throw("no matching function clause")
    end;
call(NonCallable, _) ->
    throw(lists:flatten(io_lib:format("'~p' not callable", [NonCallable]))).

last_wrap_tuple_callable({function, Argument, Body, Closure}) ->
    {function, Argument, {{'<-', nil}, {name, nil, '_'}, Body}, Closure}.
