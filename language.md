Everything is an expression
Functions are first class (and they have closures)
Atoms are a thing (erlang style)
Lists are a thing (erlang style)
Tuples are a thing (erlang style)
Records are a thing (erlang style)
Pattern matching is a thing

It's basically erlang without the OTP fun but slower


Example:

```
do_stuff(X) ->
	X + 2

print(do_stuff(1))
```

