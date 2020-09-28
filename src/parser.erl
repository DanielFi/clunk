-module(parser).
-export([parse/1, parse_and_scan/1, format_error/1]).

-file("/usr/lib/erlang/lib/parsetools-2.1.8/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error: Stacktrace ->
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write_atom(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~tp", [Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) -> [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write_atom(Other);
yecctoken2string(Other) ->
    io_lib:format("~tp", [Other]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("/home/daniel/Projects/clunk/src/parser.erl", 176).

-dialyzer({nowarn_function, yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

-dialyzer({nowarn_function, yeccpars2_0/7}).
yeccpars2_0(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_2/7}).
yeccpars2_2(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_2(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_3(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_3(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_3/7}).
yeccpars2_cont_3(S, '!=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_3(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_3(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_3(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_3(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_3(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_3(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_3(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_3(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_3(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_3(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_3(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_3(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_3(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_3(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_3(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_4: see yeccpars2_0

%% yeccpars2_5: see yeccpars2_0

yeccpars2_6(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_11(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_12(S, '!=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_12_(Stack),
 yeccgoto_content_expressions(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_13/7}).
yeccpars2_13(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_14_(Stack),
 yeccgoto_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_15_(Stack),
 yeccgoto_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_16: see yeccpars2_0

%% yeccpars2_17: see yeccpars2_0

%% yeccpars2_18: see yeccpars2_0

%% yeccpars2_19: see yeccpars2_0

%% yeccpars2_20: see yeccpars2_0

%% yeccpars2_21: see yeccpars2_0

%% yeccpars2_22: see yeccpars2_0

%% yeccpars2_23: see yeccpars2_0

%% yeccpars2_24: see yeccpars2_0

%% yeccpars2_25: see yeccpars2_0

%% yeccpars2_26: see yeccpars2_0

%% yeccpars2_27: see yeccpars2_0

%% yeccpars2_28: see yeccpars2_0

%% yeccpars2_29: see yeccpars2_0

%% yeccpars2_30: see yeccpars2_0

%% yeccpars2_31: see yeccpars2_0

yeccpars2_32(S, '!=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_32_(Stack),
 yeccgoto_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_33_(Stack),
 yeccgoto_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_34(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_34_\'$end\''(Stack),
 yeccgoto_expression(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_34(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_34_\'(\''(Stack),
 yeccgoto_expression(hd(Nss), '(', Nss, NewStack, T, Ts, Tzr);
yeccpars2_34(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_34_\')\''(Stack),
 yeccgoto_expression(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_34(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_34_\',\''(Stack),
 yeccgoto_expression(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_34(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_34_\';\''(Stack),
 yeccgoto_expression(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_34(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_34_\'=\''(Stack),
 yeccgoto_expression(hd(Nss), '=', Nss, NewStack, T, Ts, Tzr);
yeccpars2_34(_S, ']', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_34_\']\''(Stack),
 yeccgoto_expression(hd(Nss), ']', Nss, NewStack, T, Ts, Tzr);
yeccpars2_34(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_34_\'|\''(Stack),
 yeccgoto_expression(hd(Nss), '|', Nss, NewStack, T, Ts, Tzr);
yeccpars2_34(_S, '}', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_34_\'}\''(Stack),
 yeccgoto_expression(hd(Nss), '}', Nss, NewStack, T, Ts, Tzr);
yeccpars2_34(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_35(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_35_\'$end\''(Stack),
 yeccgoto_expression(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_35_\'(\''(Stack),
 yeccgoto_expression(hd(Nss), '(', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_35_\')\''(Stack),
 yeccgoto_expression(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_35_\',\''(Stack),
 yeccgoto_expression(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_35_\';\''(Stack),
 yeccgoto_expression(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_35_\'=\''(Stack),
 yeccgoto_expression(hd(Nss), '=', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, ']', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_35_\']\''(Stack),
 yeccgoto_expression(hd(Nss), ']', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_35_\'|\''(Stack),
 yeccgoto_expression(hd(Nss), '|', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, '}', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_35_\'}\''(Stack),
 yeccgoto_expression(hd(Nss), '}', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_36(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_36_\'$end\''(Stack),
 yeccgoto_expression(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_36(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_36_\'(\''(Stack),
 yeccgoto_expression(hd(Nss), '(', Nss, NewStack, T, Ts, Tzr);
yeccpars2_36(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_36_\')\''(Stack),
 yeccgoto_expression(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_36(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_36_\',\''(Stack),
 yeccgoto_expression(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_36(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_36_\';\''(Stack),
 yeccgoto_expression(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_36(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_36_\'=\''(Stack),
 yeccgoto_expression(hd(Nss), '=', Nss, NewStack, T, Ts, Tzr);
yeccpars2_36(_S, ']', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_36_\']\''(Stack),
 yeccgoto_expression(hd(Nss), ']', Nss, NewStack, T, Ts, Tzr);
yeccpars2_36(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_36_\'|\''(Stack),
 yeccgoto_expression(hd(Nss), '|', Nss, NewStack, T, Ts, Tzr);
yeccpars2_36(_S, '}', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_36_\'}\''(Stack),
 yeccgoto_expression(hd(Nss), '}', Nss, NewStack, T, Ts, Tzr);
yeccpars2_36(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_37(S, '!=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_37_(Stack),
 yeccgoto_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_38(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_38_\'$end\''(Stack),
 yeccgoto_expression(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_38(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_38_\'(\''(Stack),
 yeccgoto_expression(hd(Nss), '(', Nss, NewStack, T, Ts, Tzr);
yeccpars2_38(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_38_\')\''(Stack),
 yeccgoto_expression(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_38(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_38_\',\''(Stack),
 yeccgoto_expression(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_38(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_38_\';\''(Stack),
 yeccgoto_expression(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_38(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_38_\'=\''(Stack),
 yeccgoto_expression(hd(Nss), '=', Nss, NewStack, T, Ts, Tzr);
yeccpars2_38(_S, ']', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_38_\']\''(Stack),
 yeccgoto_expression(hd(Nss), ']', Nss, NewStack, T, Ts, Tzr);
yeccpars2_38(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_38_\'|\''(Stack),
 yeccgoto_expression(hd(Nss), '|', Nss, NewStack, T, Ts, Tzr);
yeccpars2_38(_S, '}', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_38_\'}\''(Stack),
 yeccgoto_expression(hd(Nss), '}', Nss, NewStack, T, Ts, Tzr);
yeccpars2_38(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_39(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_39_\'$end\''(Stack),
 yeccgoto_expression(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_39(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_39_\'(\''(Stack),
 yeccgoto_expression(hd(Nss), '(', Nss, NewStack, T, Ts, Tzr);
yeccpars2_39(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_39_\')\''(Stack),
 yeccgoto_expression(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_39(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_39_\',\''(Stack),
 yeccgoto_expression(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_39(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_39_\';\''(Stack),
 yeccgoto_expression(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_39(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_39_\'=\''(Stack),
 yeccgoto_expression(hd(Nss), '=', Nss, NewStack, T, Ts, Tzr);
yeccpars2_39(_S, ']', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_39_\']\''(Stack),
 yeccgoto_expression(hd(Nss), ']', Nss, NewStack, T, Ts, Tzr);
yeccpars2_39(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_39_\'|\''(Stack),
 yeccgoto_expression(hd(Nss), '|', Nss, NewStack, T, Ts, Tzr);
yeccpars2_39(_S, '}', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_39_\'}\''(Stack),
 yeccgoto_expression(hd(Nss), '}', Nss, NewStack, T, Ts, Tzr);
yeccpars2_39(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_40(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_40_(Stack),
 yeccgoto_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_41(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_41_(Stack),
 yeccgoto_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_42_(Stack),
 yeccgoto_content_expressions(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_43(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_43_(Stack),
 yeccgoto_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_44(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_(Stack),
 yeccgoto_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_45/7}).
yeccpars2_45(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_46_(Stack),
 yeccgoto_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_47(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_47_(Stack),
 yeccgoto_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_48(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_48_\'$end\''(Stack),
 yeccgoto_expression(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_48(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_48_\'(\''(Stack),
 yeccgoto_expression(hd(Nss), '(', Nss, NewStack, T, Ts, Tzr);
yeccpars2_48(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_48_\')\''(Stack),
 yeccgoto_expression(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_48(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_48_\',\''(Stack),
 yeccgoto_expression(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_48(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_48_\';\''(Stack),
 yeccgoto_expression(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_48(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_48_\'=\''(Stack),
 yeccgoto_expression(hd(Nss), '=', Nss, NewStack, T, Ts, Tzr);
yeccpars2_48(_S, ']', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_48_\']\''(Stack),
 yeccgoto_expression(hd(Nss), ']', Nss, NewStack, T, Ts, Tzr);
yeccpars2_48(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_48_\'|\''(Stack),
 yeccgoto_expression(hd(Nss), '|', Nss, NewStack, T, Ts, Tzr);
yeccpars2_48(_S, '}', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_48_\'}\''(Stack),
 yeccgoto_expression(hd(Nss), '}', Nss, NewStack, T, Ts, Tzr);
yeccpars2_48(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_49/7}).
yeccpars2_49(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_50_(Stack),
 yeccgoto_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_51_(Stack),
 yeccgoto_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_52(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_52_(Stack),
 yeccgoto_uminus(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_53(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_3(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_54/7}).
yeccpars2_54(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_55/7}).
yeccpars2_55(S, '=>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_56: see yeccpars2_0

yeccpars2_57(S, '!=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_57_(Stack),
 yeccgoto_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_58_(Stack),
 yeccgoto_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_59: see yeccpars2_0

yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_60_(Stack),
 yeccgoto_expressions(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_61(S, '!=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_61_(Stack),
 yeccgoto_expressions(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_content_expressions/7}).
yeccgoto_content_expressions(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_content_expressions(6, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_content_expressions(11, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_content_expressions(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_content_expressions(21=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expression/7}).
yeccgoto_expression(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(5, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(6, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(11, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(20, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(21, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(22, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(24, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(26, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(28, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(56, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_expressions/7}).
yeccgoto_expressions(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expressions(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_uminus/7}).
yeccgoto_uminus(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(6=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(16=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(20=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(21=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(22=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(26=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(56=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_uminus(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_12_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 50).
yeccpars2_12_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_14_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 19).
yeccpars2_14_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_15_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 20).
yeccpars2_15_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , __2 }
  end | __Stack].

-compile({inline,yeccpars2_32_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 39).
yeccpars2_32_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_33_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 32).
yeccpars2_33_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_34_\'$end\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 38).
'yeccpars2_34_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_34_\'(\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 38).
'yeccpars2_34_\'(\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_34_\')\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 38).
'yeccpars2_34_\')\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_34_\',\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 38).
'yeccpars2_34_\',\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_34_\';\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 38).
'yeccpars2_34_\';\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_34_\'=\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 38).
'yeccpars2_34_\'=\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_34_\']\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 38).
'yeccpars2_34_\']\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_34_\'|\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 38).
'yeccpars2_34_\'|\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_34_\'}\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 38).
'yeccpars2_34_\'}\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_35_\'$end\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 36).
'yeccpars2_35_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_35_\'(\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 36).
'yeccpars2_35_\'(\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_35_\')\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 36).
'yeccpars2_35_\')\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_35_\',\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 36).
'yeccpars2_35_\',\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_35_\';\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 36).
'yeccpars2_35_\';\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_35_\'=\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 36).
'yeccpars2_35_\'=\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_35_\']\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 36).
'yeccpars2_35_\']\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_35_\'|\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 36).
'yeccpars2_35_\'|\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_35_\'}\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 36).
'yeccpars2_35_\'}\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_36_\'$end\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 33).
'yeccpars2_36_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_36_\'(\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 33).
'yeccpars2_36_\'(\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_36_\')\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 33).
'yeccpars2_36_\')\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_36_\',\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 33).
'yeccpars2_36_\',\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_36_\';\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 33).
'yeccpars2_36_\';\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_36_\'=\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 33).
'yeccpars2_36_\'=\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_36_\']\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 33).
'yeccpars2_36_\']\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_36_\'|\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 33).
'yeccpars2_36_\'|\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_36_\'}\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 33).
'yeccpars2_36_\'}\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_37_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 40).
yeccpars2_37_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_38_\'$end\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 37).
'yeccpars2_38_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_38_\'(\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 37).
'yeccpars2_38_\'(\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_38_\')\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 37).
'yeccpars2_38_\')\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_38_\',\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 37).
'yeccpars2_38_\',\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_38_\';\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 37).
'yeccpars2_38_\';\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_38_\'=\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 37).
'yeccpars2_38_\'=\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_38_\']\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 37).
'yeccpars2_38_\']\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_38_\'|\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 37).
'yeccpars2_38_\'|\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_38_\'}\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 37).
'yeccpars2_38_\'}\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_39_\'$end\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 35).
'yeccpars2_39_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_39_\'(\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 35).
'yeccpars2_39_\'(\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_39_\')\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 35).
'yeccpars2_39_\')\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_39_\',\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 35).
'yeccpars2_39_\',\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_39_\';\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 35).
'yeccpars2_39_\';\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_39_\'=\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 35).
'yeccpars2_39_\'=\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_39_\']\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 35).
'yeccpars2_39_\']\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_39_\'|\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 35).
'yeccpars2_39_\'|\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_39_\'}\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 35).
'yeccpars2_39_\'}\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_40_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 30).
yeccpars2_40_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_41_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 28).
yeccpars2_41_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_42_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 51).
yeccpars2_42_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_43_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 27).
yeccpars2_43_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 29).
yeccpars2_44_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_46_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 46).
yeccpars2_46_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { call , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 31).
yeccpars2_47_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_48_\'$end\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 34).
'yeccpars2_48_\'$end\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_48_\'(\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 34).
'yeccpars2_48_\'(\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_48_\')\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 34).
'yeccpars2_48_\')\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_48_\',\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 34).
'yeccpars2_48_\',\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_48_\';\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 34).
'yeccpars2_48_\';\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_48_\'=\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 34).
'yeccpars2_48_\'=\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_48_\']\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 34).
'yeccpars2_48_\']\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_48_\'|\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 34).
'yeccpars2_48_\'|\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_48_\'}\''/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 34).
'yeccpars2_48_\'}\''(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_50_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 22).
yeccpars2_50_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { list , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_51_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 23).
yeccpars2_51_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { list , __2 }
  end | __Stack].

-compile({inline,yeccpars2_52_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 48).
yeccpars2_52_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_57_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 44).
yeccpars2_57_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { function , __2 , __5 }
  end | __Stack].

-compile({inline,yeccpars2_58_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 25).
yeccpars2_58_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_60_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 12).
yeccpars2_60_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { expressions , Es } = __3 , { expressions , [ __1 | Es ] }
  end | __Stack].

-compile({inline,yeccpars2_61_/1}).
-file("/home/daniel/Projects/clunk/src/parser.yrl", 11).
yeccpars2_61_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { expressions , [ __1 , __3 ] }
  end | __Stack].


