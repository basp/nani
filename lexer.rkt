#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         srfi/13 
         rackunit)

(define-tokens literals
  (INT
   FLOAT
   STRING
   OBJECT
   ID
   ERROR
   TYPE))

(define-empty-tokens punctuation
  (+
   -
   NEG
   *
   /
   %
   ^
   !
   =
   EQ
   <
   NE
   >
   LE
   GE
   ?
   PIPE
   =>
   $
   :
   SEMICOLON   
   COMMA
   DOT ..
   BACKTICK
   SQUOTE
   LPAREN
   RPAREN
   LBRACE
   RBRACE
   LBRACK
   RBRACK
   EOF))

(define-empty-tokens keywords
  (IN AND OR
      TRUE FALSE
      FOR ENDFOR
      WHILE ENDWHILE
      BREAK CONTINUE RETURN
      IF ELSEIF ELSE ENDIF
      TRY EXCEPT FINALLY ENDTRY
      ANY))

(define-lex-abbrevs
  [digit (:/ #\0 #\9)]
  [digit+ (:+ digit)]
  [digit* (:* digit)]
  [objid (:seq "#" (:? "-") digit+)]
  [alpha (:or (:/ #\A #\Z) (:/ #\a #\z) "_")]
  [alphanumeric (:or alpha digit)]
  [name (:seq alpha (:* alphanumeric))]
  [string (:seq #\" (:* any-char) #\")]
  [float (:or (:seq digit* "." digit+)
              (:seq digit* (:? ".") digit+ "e" (:? "-") digit+))]
  [int digit+])

(define get-string-token
  (lexer
   [(:~ #\" #\\) (cons (car (string->list lexeme))
                       (get-string-token input-port))]
   [(:: #\\ #\\) (cons #\\ (get-string-token input-port))]
   [(:: #\\ #\") (cons #\" (get-string-token input-port))]
   [#\" null]))

(define nani-lex
  (lexer-src-pos
   [(eof) 'EOF]
   [whitespace (return-without-pos (nani-lex input-port))]
   ["(" 'LPAREN]
   [")" 'RPAREN]
   ["{" 'LBRACE]
   ["}" 'RBRACE]
   ["[" 'LBRACK]
   ["]" 'RBRACK]
   ["|" 'PIPE]
   [";" 'SEMICOLON]
   ["," 'COMMA]
   ["." 'DOT]
   ["`" 'BACKTICK]
   ["'" 'SQUOTE]
   [(char-set "=<>+-*/%^!?:$") (string->symbol lexeme)]
   [".." (string->symbol lexeme)]
   ["=>" (string->symbol lexeme)]
   ["and" 'AND]
   ["or" 'OR]
   ["in" 'IN]
   ["true" 'TRUE]
   ["false" 'FALSE]
   ["return" 'RETURN]
   [name (token-ID lexeme)]
   [float (token-FLOAT (string->number lexeme))]
   [int (token-INT (string->number lexeme))]
   [objid (token-OBJECT (string->number (substring lexeme 1)))]
   [#\" (token-STRING (list->string (get-string-token input-port)))]))

(provide nani-lex
         literals
         punctuation
         keywords)

(module+ test
  (define (lex/port ip)
    (position-token-token (nani-lex ip)))
  (define (lex/string str)
    (define ip (open-input-string str))
    (lex/port ip))
  (test-begin
   (check-equal? (lex/string "#123")
                 (token-OBJECT 123))
   (check-equal? (lex/string "#-123")
                 (token-OBJECT -123)))
  (test-begin
   (define ip (open-input-string ""))
   (check-equal? (lex/port ip) 'EOF))
  (test-begin
   (define ip (open-input-string "\"foo \\\" bar\" quux"))
   (check-equal? (lex/port ip) (token-STRING "foo \" bar"))
   (check-equal? (lex/port ip) (token-ID "quux")))
  (test-begin
   (define ip (open-input-string "(){}[]"))
   (check-equal? (lex/port ip) 'LPAREN)
   (check-equal? (lex/port ip) 'RPAREN)
   (check-equal? (lex/port ip) 'LBRACE)
   (check-equal? (lex/port ip) 'RBRACE)
   (check-equal? (lex/port ip) 'LBRACK)
   (check-equal? (lex/port ip) 'RBRACK))  
  (test-begin
   (define ip (open-input-string "in and or true false return"))
   (check-equal? (lex/port ip) 'IN)
   (check-equal? (lex/port ip) 'AND)
   (check-equal? (lex/port ip) 'OR)
   (check-equal? (lex/port ip) 'TRUE)
   (check-equal? (lex/port ip) 'FALSE)
   (check-equal? (lex/port ip) 'RETURN))
  (test-begin
   (define ip (open-input-string "123 .5 1.5 1e5 1e-5 .1e-5"))
   (check-equal? (lex/port ip) (token-INT 123))
   (check-equal? (lex/port ip) (token-FLOAT 0.5))
   (check-equal? (lex/port ip) (token-FLOAT 1.5))
   (check-equal? (lex/port ip) (token-FLOAT 1e5))
   (check-equal? (lex/port ip) (token-FLOAT 1e-5))
   (check-equal? (lex/port ip) (token-FLOAT 1e-6))))
