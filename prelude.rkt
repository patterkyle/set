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
