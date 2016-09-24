# Slartibartfass

This project is a simple [Scheme][scheme] like [Lisp][lisp] interpreter.
It is based on [this tutorial][mumbler]. Also it is influenced by the
book [Structure and Interpretation of Computer Programs][saiocp].

Until now I did lot of parsing/lexing stuff, but no interpretation yet.
So here I focus on interpretation and the whole parsing is done by 
[ANTLR4][antlr].

## Install And Run

[Here is a prebuilt][dist] version. You only need [Java 8][jdk] installed
to run it.
 
For building it from scratch you need to checkout this repository and 
build it by your self. You need at least [Java 8][jdk] and [Maven 3.1][mvn].
To build chnage into the cloned repository and execute Maven:

    $> mvn clean install

After that you can run Slartibartfass:
   
    $> ./bin/slarti -h

## Use the REPL

Slartibartfass provides a Read Eval Print Loop. Just run it without any
argument to launch the REPL:

    $> ./bin/slarti

The REPL provides some special commands. They all start with a bang (`!`).
Just type `!help` to get a full list of available commands with explanation.

### The Hello, World! Example

The REPL will great you with the `sl>` prompt. Just type:

    sl> (println "Hello, World!")

and hit return to see it.
    
##  The Syntax

The full description of the Slartibartfass syntax is [Scheme][scheme] 
like and you described [here][syntax] as [ANTLR][antlr] grammar.

The basic syntax is build by lists, obviously:

    (foo bar baz)

Also an important building block are symbols. Symbols are just names
like the `foo`, `bar` and `baz` in the previous example. The first 
symbol in a list (the head of the list) is interpreted as function
name. In the previous example the interpreter looks in the scope for
a function named `foo` and applys to it the evaluated symbols `bar`
and `baz`. Evaluating a symbol means it is looked up in the scope
and that value is used. Here a more complex example:

    (define a 3)
    (define b 2)
    (println (+ a b))

This will print: `5`

### Builtin Functions

Builtin functions are directly interpreted in the interpreter (in contrast
to function provided by the standard lib).

- `+`: Sums up the given arguments. It returns 0 if no argument is given 
       and the argument itself if only one argument is given. You can pass
       as many arguments as you want.
- `-`: Subtracts the givne arguments. Throws an error if no argument is 
       given. Negates the argument if only one argument is given. You can pass
       as many arguments as you want.
- `*`: Multiplies the given arguments. Returns 1 if no argument is given.
       Return the argument itself if only one is given. You can pass
       as many arguments as you want.
- `/`: Divide numbers. This function requires at least two arguments.
- `%`: Remainder of numbers. This function requires at least two arguments.
- `<`: Compares two numbers if the first argument is less than the second. 
       Requires exactly two arguments.
- `>`: Compares two numbers if the first argument is greater than the second. 
       Requires exactly two arguments.
- `=`: Compares two arguments if the first argument is equal the second. 
       Requires exactly two arguments.
- `and`: Boolean and operation. Requires exactly two arguments. 
- `or`: Boolean or operation. Requires exactly two arguments.
- `println`: Prints the given arguments concatenated with a newline at the end.
- `print`: Prints the given arguments concatenated.

## Other Reading Stuff

- [Lambda Papers](http://library.readscheme.org/page1.html)

[antlr]:    http://www.antlr.org/
[dist]:     https://ci.weltraumschaf.de/job/Slartibartfass/lastSuccessfulBuild/artifact/target/slartibartfass.zip
[jdk]:      http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html
[lisp]:     https://en.wikipedia.org/wiki/Lisp_(programming_language)
[mumbler]:  http://cesquivias.github.io/blog/2014/10/13/writing-a-language-in-truffle-part-1-a-simple-slow-interpreter/
[mvn]:      https://maven.apache.org/download.cgi
[saiocp]:   https://mitpress.mit.edu/sicp/full-text/book/book.html
[scheme]:   https://en.wikipedia.org/wiki/Scheme_(programming_language)
[syntax]:   https://github.com/Weltraumschaf/slartibartfass/blob/master/src/main/antlr4/Slarti.g4