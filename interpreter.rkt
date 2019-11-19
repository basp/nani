#lang racket/base

(require "ast.rkt")

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
  ; TODO
  ; should expect to match
  ; both obj and name with
  ; id/object or an expr (in the case
  ; of a dynamic lookup)
  ; if dynamic lookup does not
  ; eval to a string E_INVARG will
  ; be returned
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

(define (eval x)
  (cond
    [(expr-const? x)
     (expr-const-val x)]
    [(expr-id? x)
     (var-ref (expr-id-name x))]
    [(expr-set!? x)
     (handle-set! x)]))

(provide eval)