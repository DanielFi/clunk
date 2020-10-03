-module(interpreter).

%% API exports
-export([interpret/1,interpret/2]).

-record(scope, {parent_scope = nil, bound_names = #{}}).
-record(state, {scopes = #{0 => #scope{}}, scope = 0, scope_count = 1}).

%%====================================================================
%% API functions
%%====================================================================

interpret(Node) -> 
    interpret(Node, #state{}).

interpret({expressions, Expressions}, State) -> 
    lists:foldl(fun(E, {_, S}) -> 
            interpret(E, S)
        end, {nil, State}, Expressions);

interpret({integer, _, Value}, State) ->
    {Value, State};
interpret({float, _, Value}, State) ->
    {Value, State};
interpret({atom, _, Value}, State) ->
    {Value, State};
interpret({tuple, Elements}, State) ->
    {ReversedValues, NewState} = lists:foldl(fun(E, {Vs, S}) -> 
        {V, NewS} = interpret(E, S),
        {[V | Vs], NewS}
    end, {[], State}, Elements),
    {{tuple, lists:reverse(ReversedValues)}, NewState};
interpret({list, Elements}, State) ->
    {ReversedValues, NewState} = lists:foldl(fun(E, {Vs, S}) -> 
        {V, NewS} = interpret(E, S),
        {[V | Vs], NewS}
    end, {[], State}, Elements),
    {{list, lists:reverse(ReversedValues)}, NewState};
interpret({name, _, Name}, State) ->
    case get(Name, State) of
        {ok, Value} ->
            {Value, State};
        {error, not_found} ->
            {{name, Name}, State}
    end;

interpret({{'+', _}, Left, Right}, State) ->
    {LeftValue, State1} = interpret(Left, State),
    {RightValue, State2} = interpret(Right, State1),
    {LeftValue + RightValue, State2};
interpret({{'-', _}, Left, Right}, State) ->
    {LeftValue, State1} = interpret(Left, State),
    {RightValue, State2} = interpret(Right, State1),
    {LeftValue - RightValue, State2};
interpret({{'*', _}, Left, Right}, State) ->
    {LeftValue, State1} = interpret(Left, State),
    {RightValue, State2} = interpret(Right, State1),
    {LeftValue * RightValue, State2};
interpret({{'/', _}, Left, Right}, State) ->
    {LeftValue, State1} = interpret(Left, State),
    {RightValue, State2} = interpret(Right, State1),
    {LeftValue / RightValue, State2};
interpret({{'%', _}, Left, Right}, State) ->
    {LeftValue, State1} = interpret(Left, State),
    {RightValue, State2} = interpret(Right, State1),
    {LeftValue rem RightValue, State2};
interpret({{'^', _}, Left, Right}, State) ->
    {LeftValue, State1} = interpret(Left, State),
    {RightValue, State2} = interpret(Right, State1),
    {math:pow(LeftValue, RightValue), State2};

interpret({{'=', _}, Left, Right}, State) ->
    {LeftValue, State1} = interpret(Left, State),
    {RightValue, State2} = interpret(Right, State1),
    match(LeftValue, RightValue, State2);

interpret({{'=>', _}, Argument, Body}, State) ->
    {ArgumentValue, #state{scope = Scope} = NewState} = interpret(Argument, State),
    {{function, ArgumentValue, Body, Scope}, NewState};

interpret({call, Callable, Parameter}, State) ->
    {ParameterValue, State1} = interpret(Parameter, State),
    {CallableValue, State2} = interpret(Callable, State1),
    {Result, State3} = call(CallableValue, ParameterValue, State2),
    {Result, State3#state{scope = State#state.scope}}.

%%====================================================================
%% Internal functions
%%====================================================================

get_scope(Scope, #state{scopes = Scopes}) ->
    maps:get(Scope, Scopes).

current_scope(#state{scope = Scope} = State) ->
    get_scope(Scope, State).

update_current_scope(NewScope, #state{scope = Scope, scopes = Scopes} = State) ->
    %io:format("update scope ~p~n state ~p~n", [NewScope, State]),
    State#state{scopes = Scopes#{Scope => NewScope}}.

enter_scope(ParentScope, #state{scopes = Scopes, scope_count = Count}) ->
    NewScope = #scope{parent_scope = ParentScope},
    #state{scope = Count, scopes = Scopes#{Count => NewScope}, scope_count = Count + 1}.

get(Name, #state{} = State) ->
    get(Name, current_scope(State), State).

get(Name, #scope{parent_scope = Parent, bound_names = BoundNames}, State) ->
    case {maps:is_key(Name, BoundNames), Parent} of
        {true, _} ->
            {ok, maps:get(Name, BoundNames)};
        {false, nil} ->
            {error, not_found};
        {false, _} ->
            get(Name, get_scope(Parent, State), State)
    end.

match(Left, {name, RName}, State) ->
    case get(RName, State) of
        {ok, RValue} ->
            match(Left, RValue, State);
        {error, not_found} ->
            throw(io_lib:format("name '~p' is not bound", [RName]))
    end;
match({name, LName}, Right, State) ->
    #scope{bound_names = BoundNames} = current_scope(State),
    case get(LName, State) of
        {ok, LValue} ->
            match(LValue, Right, State);
        {error, not_found} ->
            Scope = current_scope(State),
            NewScope = Scope#scope{bound_names = BoundNames#{LName => Right}},
            NewState = update_current_scope(NewScope, State),
            {Right, NewState}
    end;
match(Left, Right, State) when Left == Right andalso not is_tuple(Left)->
    {Left, State};
match({tuple, LValues}, {tuple, RValues}, State) when length(LValues) == length(RValues) ->
    {ReversedValues, NewState} = lists:foldl(fun({L, R}, {V, S}) -> 
        {NewV, NewS} = match(L, R, S), {[NewV | V], NewS} 
    end, {[], State}, lists:zip(LValues, RValues)),
    {{tuple, lists:reverse(ReversedValues)}, NewState};
match({list, LValues}, {list, RValues}, State) when length(LValues) == length(RValues) ->
    {ReversedValues, NewState} = lists:foldl(fun({L, R}, {V, S}) -> 
        {NewV, NewS} = match(L, R, S), {[NewV | V], NewS} 
    end, {[], State}, lists:zip(LValues, RValues)),
    {{list, lists:reverse(ReversedValues)}, NewState};
match(Left, Right, _) ->
    throw(lists:flatten(io_lib:format("'~p' and '~p' do not match", [Left, Right]))).

call({function, Argument, Body, Closure}, Parameter, State) ->
    CallState = enter_scope(Closure, State),
    {_, CallState1} = match(Argument, Parameter, CallState),
    interpret(Body, CallState1);
call({tuple, Callables}, Parameter, State) ->
    {Success, FinalResult} = lists:foldl(fun(Callable, {Matched, Result}) ->
        case Matched of
            true ->
                {true, Result};
            false ->
                try {true, call(Callable, Parameter, State)}
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
call(NonCallable, _, _) ->
    throw(lists:flatten(io_lib:format("'~p' not callable", [NonCallable]))).
