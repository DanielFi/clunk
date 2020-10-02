Nonterminals expressions expression uminus content_expressions tuple call.
Terminals integer float atom name ';' '+' '-' '*' '/' '^' '%' '=' '(' ')' '[' ']' '{' '}' '|' '<' '>' '==' '<=' '>=' '!=' ',' '->' '<-'.
Rootsymbol expressions.

Right 100 ','.
Right 200 '=' '<-'.
Right 300 '|'.
Nonassoc 400 '->'.
Nonassoc 500 '==' '!=' '<' '>' '<=' '>='.
Left 600 '+' '-'.
Left 700 '*' '/' '%'.
Unary 800 uminus.
Left 900 '^'.
Left 1000 '(' '{'.

expressions -> expression ';' : {expressions, ['$1']}. 
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
expression -> expression '<-' expression : {'$2', '$1', '$3'}.
expression -> expression '->' expression : {'$2', '$1', '$3'}.

expression -> uminus : '$1'.

expression -> call : '$1'.

call -> expression '(' expression ')' : {call, '$1', '$3'}.
call -> expression tuple : {call, '$1', '$2'}.

tuple -> '{' '}' : {tuple, []}.
tuple -> '{' content_expressions '}' : {tuple, '$2'}.

uminus -> '-' expression : {'$1', '$2'}.

content_expressions -> expression : ['$1'].
content_expressions -> expression ',' content_expressions : ['$1' | '$3'].