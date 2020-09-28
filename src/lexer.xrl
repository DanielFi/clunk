Definitions.

Digit = [0-9]
Lowercase = [a-z]
Uppercase = [A-Z]
Alphanumeric = [a-zA-Z_0-9]
Whitespace = [\s\t\r\n]

Rules.

{Digit}+ : 
    {token, {integer, TokenLine, list_to_integer(TokenChars)}}.

{Digit}+\.{Digit}+ :
    {token, {float, TokenLine, list_to_float(TokenChars)}}.

{Lowercase}{Alphanumeric}* :
    {token, {atom, TokenLine, list_to_atom(TokenChars)}}.

({Uppercase}|_){Alphanumeric}* :
    {token, {name, TokenLine, list_to_atom(TokenChars)}}.

(\;|\+|-|\*|/|\^|%|=|\(|\)|\[|\]|\{|\}|\||<|>|==|!=|<=|>=|,|=>) :
    {token, {list_to_atom(TokenChars), TokenLine}}.

{Whitespace} : 
    skip_token.

{Digit}+{Alphanumeric} : 
    {error, "Unexpected character " ++ lists:last(TokenChars)}.

Erlang code.