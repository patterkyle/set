#lang racket

(require "prelude.rkt")

(provide (struct-out card)
          make-deck
          all-equal?
          all-distinct?
          potential-set?
          makes-set?)

(def ([card-numbers  #(one two three)]
      [card-symbols  #(diamond squiggle oval)]
      [card-shadings #(solid striped open)]
      [card-colors   #(red green purple)]))

(struct card (number
              symbol
              shading
              color)
  #:transparent)

(define (swap-v! vec i j)
  (let ([tmp (vector-ref vec i)])
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j tmp)))

(define (shuffle-v! vec)
  (begin (for ([i (in-range (sub1 (vector-length vec)) 0 -1)])
           (define r (random (add1 i)))
           (swap-v! vec i r))
         vec))

(def (make-deck)
  (shuffle-v!
   (for*/vector ([number  card-numbers]
                 [symbol  card-symbols]
                 [shading card-shadings]
                 [color   card-colors])
     (card number symbol shading color))))

(def (all-equal? vec)
  (def first-el (vector-ref vec 0))
  (for/and ([e vec])
    (equal? e first-el)))

(def (all-distinct? vec)
  (= (vector-length vec)
     (set-count (list->set (vector->list vec)))))

(def (all-distinct-or-equal? vec)
  (or (all-equal?    vec)
      (all-distinct? vec)))

(def (potential-set? cards)
  (def cards-lst (vector->list cards))
  (for/and ([param (list (list->vector (map card-number  cards-lst))
                         (list->vector (map card-symbol  cards-lst))
                         (list->vector (map card-shading cards-lst))
                         (list->vector (map card-color   cards-lst)))])
    (all-distinct-or-equal? param)))

(def (makes-set? cards)
  (and (= (vector-length cards) 3)
       (potential-set? cards)))
