# clunk

* Everything is an expression
* Functions are first class (and they have closures)
* Function take only one argument
* Atoms are a thing (erlang style)
* Tuples are a thing
* Pattern matching is a thing
* Tuples are callable and try to call each element until a match is found

It's basically drunk erlang (without the OTP fun) with JS inspired syntax. 


### Example:

```
DoStuff = X => X + 2;

DoStuff(1).
```

See the [tests folder](tests/) for more examples. 