#lang racket

(require rackunit
         rackunit/text-ui
         "prelude.rkt"
         "logic.rkt")

(define-test-suite prelude-tests
  (test-begin
    (def apple 'banana)
    (check-equal? apple 'banana)

    (def ([cranberry 'dragonfruit]))
    (check-equal? cranberry 'dragonfruit)

    (def ([e 'frapple]
          [grapefruit 'h]))
    (check-equal? e          'frapple)
    (check-equal? grapefruit 'h)

    (def ([i 'jackfruit]
          [k 'lemon]
          [m 'n]))
    (check-equal? i 'jackfruit)
    (check-equal? k 'lemon)
    (check-equal? m 'n)

    (def ([o 'p]
          [q 'r]
          [s 't]
          [u 'v]
          [w 'x]))
    (check-equal? o 'p)
    (check-equal? q 'r)
    (check-equal? s 't)
    (check-equal? u 'v)
    (check-equal? w 'x)

    (def (foo bar)
      bar)
    (check-equal? (foo 'bar) 'bar)

    (def (foo* [baz 'baz])
      baz)
    (check-equal? (foo*) 'baz)))

(define-test-suite logic-tests
  (check-equal? (card 'one 'squiggle 'open 'green)
                (card 'one 'squiggle 'open 'green))
  (check-not-equal? (card 'one 'squiggle 'open 'green)
                    (card 'one 'squiggle 'open 'purple))
  (check-true (= 81 (length (make-deck))))
  (check-true (all-equal? '(1 1 1)))
  (check-true (not (all-equal? '(1 2 3))))
  (check-true (all-equal? (list (card 'two 'oval 'striped 'red)
                                (card 'two 'oval 'striped 'red))))
  (check-true (not (all-equal? (list (card 'two 'oval 'striped 'red)
                                     (card 'two 'diamond 'striped 'red)))))
  (check-true (all-distinct? '(1 2 3)))
  (check-true (not (all-distinct? '(1 2 3 2))))
  (check-true (all-distinct? (list (card 'two 'oval 'striped 'red)
                                   (card 'two 'oval 'striped 'green))))
  (check-true (not (all-distinct? (list (card 'two 'oval 'striped 'red)
                                        (card 'two 'oval 'striped 'red)))))
  (check-true (not (makes-set? (list (card 'two 'oval 'striped 'red)
                                     (card 'two 'oval 'striped 'green)))))
  (check-true (makes-set? (list (card 'two 'oval 'striped 'red)
                                (card 'two 'oval 'striped 'green)
                                (card 'two 'oval 'striped 'purple))))
  (check-true (makes-set? (list (card 'one 'squiggle 'striped 'red)
                                (card 'two 'oval 'solid 'green)
                                (card 'three 'diamond 'open 'purple))))
  (check-true (not (makes-set? (list (card 'three 'oval 'striped 'red)
                                     (card 'two 'oval 'striped 'green)
                                     (card 'two 'oval 'striped 'purple))))))

(run-tests prelude-tests)
(run-tests logic-tests)
