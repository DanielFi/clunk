Nonterminals program expressions expression uminus content_expressions tuple call block.
Terminals integer float atom name ';' '+' '-' '*' '/' '^' '%' '=' '(' ')' '[' ']' '{' '}' '|' '<' '>' '==' '<=' '>=' '!=' ',' '=>' '.'.
Rootsymbol program.

Unary 100 block.
Right 100 ';' ','.
Right 200 '='.
Right 300 '|'.
Right 400 '=>'.
Nonassoc 500 '==' '!=' '<' '>' '<=' '>='.
Left 600 '+' '-'.
Left 700 '*' '/' '%'.
Unary 800 uminus.
Left 900 '^'.
Unary 1000 '(' '{'.
Unary 1100 tuple '.'.

program -> expressions '.' : '$1'.

expressions -> expression : {expressions, ['$1']}. 
expressions -> expression ';' expressions : {expressions, Es} = '$3', {expressions, ['$1' | Es]}.

expression -> integer : '$1'.
expression -> float : '$1'.
expression -> atom : '$1'.
expression -> name : '$1'. 

expression -> tuple : '$1'.

expression -> '[' ']' : {list, []}.
expression -> '[' content_expressions ']' : {list, '$2'}.

expression -> '(' expression ')' : '$2'.

expression -> expression '+' expression : {'$2', '$1', '$3'}.
expression -> expression '-' expression : {'$2', '$1', '$3'}.
expression -> expression '*' expression : {'$2', '$1', '$3'}.
expression -> expression '/' expression : {'$2', '$1', '$3'}.
expression -> expression '%' expression : {'$2', '$1', '$3'}.
expression -> expression '^' expression : {'$2', '$1', '$3'}.
expression -> expression '==' expression : {'$2', '$1', '$3'}.
expression -> expression '!=' expression : {'$2', '$1', '$3'}.
expression -> expression '<' expression : {'$2', '$1', '$3'}.
expression -> expression '>' expression : {'$2', '$1', '$3'}.
expression -> expression '<=' expression : {'$2', '$1', '$3'}.
expression -> expression '>=' expression : {'$2', '$1', '$3'}.
expression -> expression '|' expression : {'$2', '$1', '$3'}.
expression -> expression '=' expression : {'$2', '$1', '$3'}.
expression -> expression '=>' expression : {'$2', '$1', '$3'}.
expression -> expression '=>' block : {'$2', '$1', '$3'}.

expression -> uminus : '$1'.

expression -> call : '$1'.

block -> '{' expressions '.' '}' : '$2'.

call -> expression tuple : {call, '$1', '$2'}.
call -> expression '(' content_expressions ')' : curry('$1', '$3').

tuple -> '{' '}' : {tuple, []}.
tuple -> '{' content_expressions '}' : {tuple, '$2'}.

uminus -> '-' expression : {'$1', '$2'}.

content_expressions -> expression : ['$1'].
content_expressions -> expression ',' content_expressions : ['$1' | '$3'].

Erlang code.

curry(Callable, [Head | Tail] = _Parameters) ->
    lists:foldl(fun(Parameter, AST) ->
        {call, AST, Parameter}
    end, {call, Callable, Head}, Tail).
