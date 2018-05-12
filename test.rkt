#lang racket

(require rackunit
         rackunit/text-ui
         "logic.rkt")

(define-test-suite set-tests
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

(run-tests set-tests)
