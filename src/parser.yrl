Nonterminals expressions expression uminus content_expressions.
Terminals integer float atom name ';' '+' '-' '*' '/' '^' '%' '=' '(' ')' '[' ']' '{' '}' '|' '<' '>' '==' '<=' '>=' '!=' ',' '=>'.
Rootsymbol expressions.

Right 100 ','.
Right 200 '='.
Right 300 '|'.
Right 400 '=>'.
Nonassoc 500 '==' '!=' '<' '>' '<=' '>='.
Left 600 '+' '-'.
Left 700 '*' '/' '%'.
Unary 800 uminus.
Left 900 '^'.

expressions -> expression ';' expression : {expressions, ['$1', '$3']}. 
expressions -> expression ';' expressions : {expressions, Es} = '$3', {expressions, ['$1' | Es]}.

expression -> integer : '$1'.
expression -> float : '$1'.
expression -> atom : '$1'.
expression -> name : '$1'. 

expression -> '{' '}' : {tuple, []}.
expression -> '{' content_expressions '}' : {tuple, '$2'}.

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

expression -> uminus : '$1'.

expression -> '(' content_expressions ')' '=>' expression : {function, '$2', '$5'}.
expression -> '(' content_expressions ')' '=>' '{' expressions '}' : {function, '$2', '$6'}.

expression -> expression '(' content_expressions ')' : {call, '$1', '$3'}.

uminus -> '-' expression : {'$1', '$2'}.

content_expressions -> expression : ['$1'].
content_expressions -> expression ',' content_expressions : ['$1' | '$2'].