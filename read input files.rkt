#lang racket
(require parser-tools/lex)
(require parser-tools/lex (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc parser-tools/lex (prefix-in : parser-tools/lex-sre) syntax/readerr)

;Tokens ----------------------------------------------------------------------------------------------------------------------------------------
(define-tokens value-tokens (CHARACTER INT PRIM ID SIGN UNOP BOOL EMPTY LPAR RPAR LBRA RBRA ENDLINE IF ELSE THEN LET IN MAP COMMA COLONEQUALS OPERATOR TO))
(define-empty-tokens op-tokens (newline EOF))

;Abbreviations ----------------------------------------------------------------------------------------------------------------------------------------
(define-lex-abbrevs
  [Character (:or (char-range #\a #\z) (char-range #\A #\Z) #\? #\_)]
  [Digit (:/ #\0 #\9)]
  [LPAR "("]
  [RPAR ")"]
  [LBRA "["]
  [RBRA "]"]
  [ENDLINE ";"]
  [COMMA ","]
  [THEN "then"]
  [LET "let"]
  [IF "if"]
  [ELSE "else"]
  [IN "in"]
  [MAP "map"]
  [TO "to"]
  [EMPTY "empty"]
  [COLONEQUALS ":="]
  [operator (:or #\* #\/ #\= "!=" "<=" ">=" #\< #\> #\& #\|)]
  [SIGN (:or "+" "-")]
  [BOOL (:or "true" "false")]
  [PRIM (:or "number?" "function?" "list?" "empty?" "cons?" "cons" "first" "rest"  "arity")]
  [UNOP "~"]
)

;Lexer -----------------------------------------------------------------------------------------------------------------------------------------
(define ciicom-lexer
  (lexer

   [(:= 1 EMPTY)
    (token-EMPTY (string->symbol lexeme))]
   
   [(:= 1 Digit)
    (token-INT (string->number lexeme))]
   
   [(:= 1 PRIM)
    (token-PRIM (string->symbol lexeme))]
   
   [(:= 1 SIGN)
    (token-SIGN (string->symbol lexeme))]
   
   [(:= 1 UNOP)
    (token-UNOP (string->symbol lexeme))]
   
   [(:= 1 BOOL)
    (token-BOOL (string->symbol lexeme))]
   
   [(:= 1 ENDLINE)
    (token-ENDLINE (string->symbol lexeme))]
   
   [(:= 1 IF)
    (token-IF (string->symbol lexeme))]
   
   [(:= 1 ELSE)
    (token-ELSE (string->symbol lexeme))]
   
   [(:= 1 LET)
    (token-LET (string->symbol lexeme))]
   
   [(:= 1 THEN)
    (token-THEN (string->symbol lexeme))]

   [(:= 1 TO)
    (token-TO (string->symbol lexeme))]
   
   [(:= 1 MAP)
    (token-MAP (string->symbol lexeme))]
   
   [(:= 1 IN)
    (token-IN (string->symbol lexeme))]

   [(:= 1 EMPTY)
    (token-EMPTY (string->symbol lexeme))]

   [(:: Character (:*(:or Character Digit)))
    (token-ID (string->symbol lexeme))]
   
   [(:= 1 COMMA)
    (token-COMMA (string->symbol lexeme))]
   
   [(:= 1 COLONEQUALS)
    (token-COLONEQUALS (string->symbol lexeme))]
   
   [(:= 1 operator)
    (token-OPERATOR (string->symbol lexeme))]

    [(:= 1 LPAR)
    (token-LPAR (string->symbol lexeme))]
   
   [(:= 1 RPAR)
    (token-RPAR (string->symbol lexeme))]
   
   [(:= 1 LBRA)
    (token-LBRA (string->symbol lexeme))]
   
   [(:= 1 RBRA)
    (token-RBRA (string->symbol lexeme))]
   
   [#\newline 
    (ciicom-lexer input-port)]

   [#\space 
    (ciicom-lexer input-port)]
   
   [(eof)
    'EOF]))

;Parser -----------------------------------------------------------------------------------------------------------------------------------------
(define ciicom-parser
  (parser
       (start exp)
       
       (end EOF)
       
       (tokens value-tokens op-tokens)

       (error void)
;        (error (lambda (tok-ok? tok-name tok-val)
;                 (cond
;                   [(equal? tok-name token-EOF) void]
;                   [else (print "error in -> ")
;                         (print tok-name)
;                         (print ": ")
;                         (print tok-val)])))
       
       (grammar
       (exp ((term binop exp) "ACTION")
            ((term) "ACTION")
            ((IF exp THEN exp ELSE exp) "ACTION")
            ((LET def IN exp) "ACTION")
            ((MAP idList TO exp) "ACTION"))
       
       (term ((unop term) "ACTION")
            ((factor) "ACTION")
            ((factor LPAR expList RPAR) "ACTION")
            ((EMPTY) "ACTION")
            ((INT) "ACTION")
            ((BOOL) "ACTION"))
       
       (factor ((LPAR exp RPAR) "ACTION")
            ((PRIM) "ACTION")
            ((ID) "ACTION"))
       
       (expList (() "ACTION")
            ((propExpList) "ACTION"))
       
       (propExpList ((exp) "ACTION")
            ((exp COMMA propExpList) "ACTION"))

       (idList (() "ACTION")
            ((propIdList) "ACTION"))

       (propIdList ((ID) "ACTION")
            ((ID COMMA propIdList) "ACTION"))

       (def ((ID COLONEQUALS exp ENDLINE def) "ACTION")
         ((ID COLONEQUALS exp ENDLINE) "ACTION"))

       (empty ((EMPTY) "ACTION"))

       (bool ((BOOL) "ACTION"))

       (unop ((SIGN) "ACTION")
            ((UNOP) "ACTION"))
       
       (binop ((SIGN) "ACTION")
            ((OPERATOR) "ACTION"))

       (prim ((PRIM) "ACTION"))

       (id ((ID) "ACTION"))

       (int ((INT) "ACTION"))

   )))

;Output -----------------------------------------------------------------------------------------------------------------------------------------
(define a (file->string "Test.lang"))
(define (ciicom-lexer-this lexer input) (lambda () (lexer input)))
(ciicom-parser (ciicom-lexer-this ciicom-lexer (open-input-string a)))

