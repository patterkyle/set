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

(def (make-deck)
  (shuffle
   (for*/list ([number  card-numbers]
               [symbol  card-symbols]
               [shading card-shadings]
               [color   card-colors])
     (card number symbol shading color))))

(def (all-equal? lst)
  (def first-el (first lst))
  (for/and ([e lst])
    (equal? e first-el)))

(def (all-distinct? lst)
  (= (length lst)
     (length (remove-duplicates lst))))

(def (all-distinct-or-equal? lst)
  (or (all-equal?    lst)
      (all-distinct? lst)))

(def (potential-set? cards)
  (for/and ([param (list (map card-number  cards)
                         (map card-symbol  cards)
                         (map card-shading cards)
                         (map card-color   cards))])
    (all-distinct-or-equal? param)))

(def (makes-set? cards)
  (and (= (length cards) 3)
       (potential-set? cards)))
