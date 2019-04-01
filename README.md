# javalisp
A simple LISP interpreter implemented in Java.

I wrote this over the course of about three days more or less as an exercise, based off my memory of LISP from before 
Java existed.  LISP is so simple a language that once you learn it, you can't really forget it, and the LISP interpreter, 
consisting primarily of two mutually recursive functions, `eval` and `apply`, is equally simple.

The parser is a simple recursive descent parser of less tha 200 lines of code, built on top of a Java StringTokenizer.
It handles lists, including dot notation, symbols, integers, strings, and bits of syntactic sugar for things like 
`'(A B C)` as a shorthand for `(QUOTE (A B C))`, and similar shorthands for `BACKQUOTE`, `COMMA` and `COMMA_AT` 
(primarily used in defining LISP macros). So far the only reason the string type was included was for the `LOAD` function,
which takes the name of a file as a string. 

Using maven, the interactive interpreter can be run by running: 

`mvn compile`

`mvn exec:java -Dexec.mainClass=lisp.Interpreter`

Exit the interactive interpreter by evaluating `NIL` or `()`.

Macro definition is supported.  There's an example of defining `LET` as a macro in the file `let_macro.lisp`.

Files can be loaded with the LOAD function.  Example:

`(LOAD "let_macro.lisp")`

There's a trace facility, which prints arguments and return values for every call to eval or apply, 
as well as macro expansion.  Tracing is started and stopped by evaluating `(TRACE)` and `(UNTRACE)`.
