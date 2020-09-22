
-record(token, {type, value}). 

-define(SINGLE_CHARACTER_OPERATORS, [
    "!",
    "$",
    "%",
    "^",
    "&",
    "*",
    "(",
    ")",
    "+",
    "-",
    "/",
    "=",
    "<",
    ">",
    "[",
    "]"
    ]).

-define(TWO_CHARACTER_OPERATORS, [
    "->",
    "==",
    ">=",
    "<=",
    "!="
    ]).
