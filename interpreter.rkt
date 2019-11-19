#lang racket/base

(require racket/match
         racket/math
         "ast.rkt"
         "parser.rkt")

(define INT 1)
(define FLOAT 2)
(define STR 3)
(define LIST 4)
(define HASH 5)
(define OBJ 6)
(define ERR 7)

(define env (make-hash))

(define (typeof xs)
  (cond
    [(integer? xs) 'INT]
    [(number? xs) 'FLOAT]
    [(string? xs) 'STR]
    [(list? xs)
     (map typeof xs)]))

(define (var-ref id)
  (cond
    [(hash-has-key? env id)
     (hash-ref env id)]
    [else
     (cons 'E_VARNF id)]))

(define (var-set! name val)
  (hash-set! env name val)
  val)

(define (prop-ref obj prop-name)
  ; TODO
  (cons 'E_PROPNF prop-name))

(define (prop-set! obj prop-name val)
  ; TODO
  (cons 'E_PROPNF prop-name))

(define (handle-set! x)
  (let ([lhs (expr-set!-lhs x)]
        [rhs (expr-set!-rhs x)])
    (cond
      [(expr-id? lhs)
       (var-set! (expr-id-name lhs)
                 (eval rhs))]
      [(expr-prop? lhs)
       (prop-set! (expr-prop-obj lhs)
                  (expr-prop-name lhs)
                  (eval rhs))]
      [(let ([dyn-lhs (eval lhs)])
         (cond
           [(string? dyn-lhs)
            (var-set! dyn-lhs
                      (eval rhs))]
           [else (cons 'E_VARNF dyn-lhs)]))])))

(define (handle-binary x)
  (define (handle-add lhs rhs)
    (let ([lhs (eval lhs)]
          [rhs (eval rhs)])
      (cond
        [(and (number? lhs) (number? rhs))
         (+ lhs rhs)]
        [(and (string? lhs) (string? rhs))
         (string-append lhs rhs)]
        [(and (number? lhs) (string? rhs))
         (string-append (number->string lhs) rhs)]
        [(and (string? lhs) (number? rhs))
         (string-append lhs (number->string rhs))]
        [else 'E_INVARG])))

  (define (handle-sub lhs rhs)
    (let ([lhs (eval lhs)]
          [rhs (eval rhs)])
      (cond
        [(and (number? lhs) (number? rhs))
         (- lhs rhs)]
        [else 'E_INVARG])))

  (define (handle-div lhs rhs)
    (let ([lhs (eval lhs)]
          [rhs (eval rhs)])
      (cond
        [(and (number? lhs) (number? rhs))
         ; (arbitrarely) force rhs to inexact so
         ; we won't have any rationals popping up
         (/ lhs (exact->inexact rhs))]
        [else 'E_INVARG])))

  (define (handle-mul lhs rhs)
    (let ([lhs (eval lhs)]
          [rhs (eval rhs)])
      (cond
        [(and (number? lhs) (number? rhs))
         (* lhs rhs)]
        [else 'E_INVARG])))

  (define (handle-mod lhs rhs)
    (let ([lhs (eval lhs)]
          [rhs (eval rhs)])
      (cond
        [(and (integer? lhs) (integer? rhs))
         (modulo lhs rhs)]
        [else 'E_INVARG])))

  (define (handle-exp lhs rhs)
    (let ([lhs (eval lhs)]
          [rhs (eval rhs)])
      (cond
        [(and (number? lhs) (number? rhs))
          (expt lhs rhs)]
        [else 'E_INVARG]))) 

  (define (handle-eq lhs rhs)
    (let ([lhs (eval lhs)]
          [rhs (eval rhs)])
      (cond
        [(and (inexact? lhs) (integer? rhs))
         (equal? lhs (exact->inexact rhs))]
        [(and (integer? lhs) (inexact? rhs))
         (equal? (exact->inexact lhs) rhs)]
        [else
         (equal? lhs rhs)])))

  (let ([op (expr-binary-op x)]
        [lhs (expr-binary-lhs x)]
        [rhs (expr-binary-rhs x)])
    (match op
      ['add (handle-add lhs rhs)]
      ['sub (handle-sub lhs rhs)]
      ['div (handle-div lhs rhs)]
      ['mul (handle-mul lhs rhs)]
      ['mod (handle-mod lhs rhs)]
      ['exp (handle-exp lhs rhs)]
      ['eq (handle-eq lhs rhs)])))

(define (handle-call x)
  (let ([fn (expr-call-fn x)]
        [args (expr-call-args x)])
    (displayln fn)
    (displayln args)))
        
    
(define (eval x)
  (cond
    [(expr-const? x)
     (expr-const-val x)]
    [(expr-id? x)
     (var-ref (expr-id-name x))]
    [(expr-set!? x)
     (handle-set! x)]
    [(expr-binary? x)
     (handle-binary x)]
    [(expr-call? x)
     (handle-call x)]))

(define (eval/parse str)
  (eval (parse/string str)))

(provide eval
         eval/parse)