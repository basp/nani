#lang racket/base

(require racket/match
         racket/math
         "ast.rkt"
         "parser.rkt")

(define env (make-hash))

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

(define (handle-add-op lhs rhs)
  (let ([dlhs (eval lhs)]
        [drhs (eval rhs)])
    (cond
      [(and (number? dlhs) (number? drhs))
       (+ dlhs drhs)]
      [(and (string? dlhs) (string? drhs))
       (string-append dlhs drhs)]
      [(and (number? dlhs) (string? drhs))
       (string-append (number->string dlhs) drhs)]
      [(and (string? dlhs) (number? drhs))
       (string-append dlhs (number->string drhs))]
      [else 'E_INVARG])))

(define (handle-binary-op x)
  (let ([op (expr-binary-op x)]
        [lhs (expr-binary-lhs x)]
        [rhs (expr-binary-rhs x)])
    (match op
      ['add (handle-add-op lhs rhs)]
      ['sub (- (eval lhs) (eval rhs))]
      ['div (exact-floor (/ (eval lhs) (eval rhs)))]
      ['mul (* (eval lhs) (eval rhs))])))

(define (eval x)
  (cond
    [(expr-const? x)
     (expr-const-val x)]
    [(expr-id? x)
     (var-ref (expr-id-name x))]
    [(expr-set!? x)
     (handle-set! x)]
    [(expr-binary? x)
     (handle-binary-op x)]))

(define (eval/parse str)
  (eval (parse/string str)))

(provide eval eval/parse)