# Slartibartfass

This project is a simple [Scheme][scheme] like [Lisp][lisp] interpreter.
It is based on [this tutorial][mumbler]. Also it is influenced by the
book [Structure and Interpretation of Computer Programs][saiocp].

Until now I did lot of parsing/lexing stuff, but no interpretation yet.
So here I focus on interpretation and the whole parsing is done by 
[ANTLR4][antlr].

[antlr]:    http://www.antlr.org/
[lisp]:     https://en.wikipedia.org/wiki/Lisp_(programming_language)
[mumbler]:  http://cesquivias.github.io/blog/2014/10/13/writing-a-language-in-truffle-part-1-a-simple-slow-interpreter/
[saiocp]:   https://mitpress.mit.edu/sicp/full-text/book/book.html
[scheme]:   https://en.wikipedia.org/wiki/Scheme_(programming_language)