grammar Slarti;

@header {
package de.weltraumschaf.slartibartfass.frontend;
}

file : form* EOF ;

form : '(' form* ')'    # list
    | '\'' form         # quote
    | INTEGER           # integer
    | REAL              # real
    | BOOLEAN           # bool
    | STRING            # string
    | SYMBOL            # symbol
    | NIL               # nil
    ;

INTEGER : DIGIT+ ;
REAL    : (DIGIT)+ '.' (DIGIT)* EXPONENT?
        | '.' (DIGIT)+ EXPONENT?
        | (DIGIT)+ EXPONENT ;
fragment
EXPONENT: ('e'|'E') ('+' | '-') ? ? DIGIT+ ;
BOOLEAN : ( '#false' | '#true' ) ;
STRING  : '"' ( ~'"' | '\\' '"')* '"' ;
SYMBOL  : ~( '#' | '"' | '\'' | [()] | [ \t\r\n] ) ~( '"' | '\'' | [()] | [ \t\r\n] )* ;
NIL     : 'nil' ;

DIGIT   : [0-9] ;
COMMENT : ';' .*? '\n' -> skip ;
WS      : [ \t\r\n] -> skip ;
