Definitions.

Digit = [0-9]
Lowercase = [a-z]
Alpha = [a-zA-Z]
Atomable = [a-zA-Z_0-9]
Whitespace = [\s\t\r\n]

Rules.

{Digit}+ : 
    {token, {integer, TokenLine, list_to_integer(TokenChars)}}.

{Digit}+\.{Digit}+ :
    {token, {float, TokenLine, list_to_float(TokenChars)}}.

{Lowercase}{Atomable}* :
    {token, {atom, TokenLine, list_to_atom(TokenChars)}}.

({Alpha)|_}{Atomable}* :
    {token, {name, TokenLine, list_to_atom(TokenChars)}}.

(\;|\+|-|\*|/|\^|%|=|\(|\)|\[|\]|\{|\}|\||<|>|==|!=|<=|>=|,|=>) :
    {token, {list_to_atom(TokenChars), TokenLine}}.

{Whitespace} : 
    skip_token.

{Digit}+{Atomable} : 
    {error, "Unexpected character " ++ lists:last(TokenChars)}.

Erlang code.