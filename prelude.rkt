#lang racket

(provide def)

(define-syntax def
  (syntax-rules ()
    [(_ id val)                        (define id val)]
    [(_ ([id val]))                    (define id val)]
    [(_ ([id0 val0] [id1 val1] ...))   (begin (define id0 val0)
                                              (def ([id1 val1] ...)))]
    [(_ (id args ...) body0 body1 ...) (define (id args ...)
                                         body0
                                         body1 ...)]))

; A version of take that doesn't throw an error when n is larger than the list.
(def (take* lst n)
  (cond [(zero? n)    empty]
        [(empty? lst) empty]
        [else         (cons (car lst)
                            (take* (cdr lst)
                                   (sub1 n)))]))
