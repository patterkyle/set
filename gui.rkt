#lang racket

(require pict
         (only-in racket/draw
                  brush%
                  dc-path%
                  pen%)
         racket/gui
         "logic.rkt")

(define deck (make-deck))

(define (diamond w h color shading)
  (define-values (pict-color pict-shading)
    (values (case color
              ['red    "red"]
              ['green  "green"]
              ['purple "purple"])
            (case shading
              ['solid   'solid]
              ['striped 'horizontal-hatch]
              ['open    'transparent])))
  (dc (λ (dc dx dy)
        (define old-brush (send dc get-brush))
        (define old-pen (send dc get-pen))
        (send dc set-brush pict-color pict-shading)
        (send dc set-pen pict-color 3 'solid)
        (define path (new dc-path%))
        (send path move-to
              (/ w 2)      0)
        (send path line-to
              w            (/ h 2))
        (send path line-to
              (/ w 2)      h)
        (send path line-to
              0            (/ h 2))
        (send path close)
        (send dc draw-path path dx dy)
        (send dc set-brush old-brush)
        (send dc set-pen old-pen))
      w h))

(define (oval w h color shading)
  (define-values (pict-color pict-shading)
    (values (case color
              ['red    "red"]
              ['green  "green"]
              ['purple "purple"])
            (case shading
              ['solid   'solid]
              ['striped 'horizontal-hatch]
              ['open    'transparent])))
  (dc (λ (dc dx dy)
        (define old-brush (send dc get-brush))
        (define old-pen (send dc get-pen))
        (send dc set-brush pict-color pict-shading)
        (send dc set-pen pict-color 3 'solid)
        (define path (new dc-path%))
        (define curve-h (/ h 8))
        (send path move-to
              0             curve-h)
        (send path curve-to
              (/ w 3)       (- (/ curve-h 5))
              (* w 2/3)     (- (/ curve-h 5))
              w             curve-h)
        (send path line-to
              w             (- h curve-h))
        (send path curve-to
              (* w 2/3)     (+ h (/ curve-h 5))
              (* w 1/3)     (+ h (/ curve-h 5))
              0             (- h curve-h))
        (send path close)
        (send dc draw-path path dx dy)
        (send dc set-brush old-brush)
        (send dc set-pen old-pen))
      w h))

(define (squiggle w h color shading)
  (define-values (pict-color pict-shading)
    (values (case color
              ['red    "red"]
              ['green  "green"]
              ['purple "purple"])
            (case shading
              ['solid   'solid]
              ['striped 'horizontal-hatch]
              ['open    'transparent])))
  (dc (λ (dc dx dy)
        (define old-brush (send dc get-brush))
        (define old-pen   (send dc get-pen))
        (send dc set-brush pict-color pict-shading)
        (send dc set-pen pict-color 3 'solid)
        (define path (new dc-path%))
        (define offset (/ h 64))
        (send path move-to
              0             offset)
        (send path curve-to
              (/ w 4)       (+ (/ offset 2))
              (* w 3/4)     (+ (/ offset 2))
              w             (/ h 4))
        (send path curve-to
              (/ w 2)       (/ h 2)
              (* w 3/4)     (* h 3/4)
              w             (- h offset))
        (send path curve-to
              (* w 3/4)     (- h (/ offset 2))
              (/ w 4)       (- h (/ offset 2))
              0             (* h 3/4))
        (send path curve-to
              (/ w 2)       (/ h 2)
              (/ w 4)       (/ h 4)
              0             offset)
        (send path close)
        (send dc draw-path path dx dy)
        (send dc set-brush old-brush)
        (send dc set-pen old-pen))
      w h))

(define (symbol-row w h symbol number color shading)
  (define symbol-pict (case symbol
                        ['diamond  diamond]
                        ['oval     oval]
                        ['squiggle squiggle]))
  (define shape-w (/ w 4))
  (define spacing (/ w 20))
   (case number
     ['one   (symbol-pict shape-w h color shading)]
     ['two   (hc-append (* spacing 2)
                        (symbol-pict shape-w h color shading)
                        (symbol-pict shape-w h color shading))]
     ['three (hc-append spacing
                        (symbol-pict shape-w h color shading)
                        (symbol-pict shape-w h color shading)
                        (symbol-pict shape-w h color shading))]))

(define (card-pict card w h)
  (cc-superimpose (filled-rectangle w h #:color "white")
                  (symbol-row w
                              (* h 0.8)
                              (card-symbol  card)
                              (card-number  card)
                              (card-color   card)
                              (card-shading card))))
