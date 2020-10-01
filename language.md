# clunk

* Everything is an expression
* Functions are first class (and they have closures)
* Atoms are a thing (erlang style)
* Tuples are a thing
* Pattern matching is a thing
* You can match anything, even functions themselves!

It's basically drunk erlang (without the OTP fun) with JS inspired syntax. 


### Example:

```
DoStuff = (X) => {
	X + 2;
};

print(DoStuff(1));
```