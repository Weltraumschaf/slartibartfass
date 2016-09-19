# Slartibartfass

This project is a simple [Scheme][scheme] like [Lisp][lisp] interpreter.
It is based on [this tutorial][mumbler]. Also it is influenced by the
book [Structure and Interpretation of Computer Programs][saiocp].

Until now I did lot of parsing/lexing stuff, but no interpretation yet.
So here I focus on interpretation and the whole parsing is done by 
[ANTLR4][antlr].

## Install And Run

There is no binary distribution! You need to checkout this repository and 
buid it by your self. You need at least [Java 8][jdk] and [Maven 3.1][mvn].
To build chnage into the cloned repository and execute Maven:

    $> mvn clean install

After that you can run Slartibartfass:
   
   $> ./bin/slarti -h

## Other Reading Stuff

- [Lambda Papers](http://library.readscheme.org/page1.html)

[antlr]:    http://www.antlr.org/
[jdk]:      TODO
[lisp]:     https://en.wikipedia.org/wiki/Lisp_(programming_language)
[mumbler]:  http://cesquivias.github.io/blog/2014/10/13/writing-a-language-in-truffle-part-1-a-simple-slow-interpreter/
[mvn]:      TODO
[saiocp]:   https://mitpress.mit.edu/sicp/full-text/book/book.html
[scheme]:   https://en.wikipedia.org/wiki/Scheme_(programming_language)