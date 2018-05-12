#lang racket

(provide make-deck
         makes-set?
         (struct-out card)
         all-equal?
         all-distinct?)

(define card-numbers  #(one two three))
(define card-symbols  #(diamond squiggle oval))
(define card-shadings #(solid striped open))
(define card-colors   #(red green purple))

(struct card (number
              symbol
              shading
              color)
  #:transparent)

(define (make-deck)
  (shuffle (for*/list ([number  card-numbers]
                       [symbol  card-symbols]
                       [shading card-shadings]
                       [color   card-colors])
             (card number symbol shading color))))

(define (all-equal? lst)
  (define first-el (first lst))
  (for/and ([e lst])
    (equal? e first-el)))

(define (all-distinct? lst)
  (= (length lst)
     (length (remove-duplicates lst))))

(define (all-distinct-or-equal? lst)
  (or (all-equal?    lst)
      (all-distinct? lst)))

(define (makes-set? cards)
  (and (= (length cards) 3)
       (for/and ([param (list (map card-number  cards)
                              (map card-symbol  cards)
                              (map card-shading cards)
                              (map card-color   cards))])
         (all-distinct-or-equal? param))))

