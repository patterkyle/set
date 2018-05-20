#lang racket

(require (only-in racket/draw
                  brush%
                  dc-path%
                  pen%)
         pict
         pict/convert
         racket/gui
         "prelude.rkt"
         "logic.rkt")

(def ([phi           (/ (+ 1 (sqrt 5)) 2)]
      [frame-w       800]
      [frame-h       (/ frame-w phi)]
      [pen-w         3]
      [hover-color   "Fuchsia"]
      [selected-color "yellow"]))

;game state
(struct gs (status
            deck
            visible-card-count
            selected-cards
            hover-idx)
  #:mutable
  #:transparent)

(def game-state (gs 'playing
                    (make-deck)
                    16
                    empty
                    0))

(def (card-color->pict-color color)
  (case color
    ['red    "red"]
    ['green  "green"]
    ['purple "purple"]))

(def (card-shading->pict-shading shading)
  (case shading
    ['solid   'solid]
    ['striped 'horizontal-hatch]
    ['open    'transparent]))

(def (diamond-path w h)
  (def path (new dc-path%))
  (send path move-to (/ w 2) 0)
  (send path line-to w       (/ h 2))
  (send path line-to (/ w 2) h)
  (send path line-to 0       (/ h 2))
  (send path close)
  path)

(def (diamond w h shading color)
  (def ([pict-color   (card-color->pict-color color)]
        [pict-shading (card-shading->pict-shading shading)]))
  (dc (λ (dc dx dy)
        (def ([old-brush (send dc get-brush)]
              [old-pen   (send dc get-pen)]))
        (send dc set-brush pict-color pict-shading)
        (send dc set-pen pict-color pen-w 'solid)
        (send dc draw-path (diamond-path w h) dx dy)
        (send dc set-brush old-brush)
        (send dc set-pen old-pen))
      w h))

(def (oval-path w h)
  (def ([path    (new dc-path%)]
        [curve-h (/ h 8)]))
  (send path move-to
        0         curve-h)
  (send path curve-to
        (/ w 3)   (- (/ curve-h 5))
        (* w 2/3) (- (/ curve-h 5))
        w         curve-h)
  (send path line-to
        w         (- h curve-h))
  (send path curve-to
        (* w 2/3) (+ h (/ curve-h 5))
        (* w 1/3) (+ h (/ curve-h 5))
        0         (- h curve-h))
  (send path close)
  path)

(def (oval w h shading color)
  (def ([pict-color   (card-color->pict-color color)]
        [pict-shading (card-shading->pict-shading shading)]))
  (dc (λ (dc dx dy)
        (def old-brush (send dc get-brush))
        (def old-pen (send dc get-pen))
        (send dc set-brush pict-color pict-shading)
        (send dc set-pen pict-color pen-w 'solid)
        (send dc draw-path (oval-path w h) dx dy)
        (send dc set-brush old-brush)
        (send dc set-pen old-pen))
      w h))

(def (squiggle-path w h)
  (def ([path   (new dc-path%)]
        [offset (/ h 64)]))
  (send path move-to
        0         offset)
  (send path curve-to
        (/ w 4)   (+ (/ offset 2))
        (* w 3/4) (+ (/ offset 2))
        w         (/ h 4))
  (send path curve-to
        (/ w 2)   (/ h 2)
        (* w 3/4) (* h 3/4)
        w         (- h offset))
  (send path curve-to
        (* w 3/4) (- h (/ offset 2))
        (/ w 4)   (- h (/ offset 2))
        0         (* h 3/4))
  (send path curve-to
        (/ w 2)   (/ h 2)
        (/ w 4)   (/ h 4)
        0         offset)
  (send path close)
  path)

(def (squiggle w h shading color)
  (def ([pict-color   (card-color->pict-color color)]
        [pict-shading (card-shading->pict-shading shading)]))
  (dc (λ (dc dx dy)
        (def ([old-brush (send dc get-brush)]
              [old-pen   (send dc get-pen)]))
        (send dc set-brush pict-color pict-shading)
        (send dc set-pen pict-color pen-w 'solid)
        (send dc draw-path (squiggle-path w h) dx dy)
        (send dc set-brush old-brush)
        (send dc set-pen old-pen))
      w h))

(def (card-symbol->pict-fn symbol)
  (case symbol
    ['diamond  diamond]
    ['oval     oval]
    ['squiggle squiggle]))

(def (symbol-row h number symbol shading color)
  (def ([w       (* h phi)]
        [pict-fn (card-symbol->pict-fn symbol)]
        [spacing (/ w 12)]
        [shape-w (/ w 3.5)]))
  (case number
    ['one   (pict-fn shape-w h shading color)]
    ['two   (hc-append (* spacing 2)
                       (pict-fn shape-w h shading color)
                       (pict-fn shape-w h shading color))]
    ['three (hc-append spacing
                       (pict-fn shape-w h shading color)
                       (pict-fn shape-w h shading color)
                       (pict-fn shape-w h shading color))]))

(def (card-pict w card
                #:selected? [selected? #f]
                #:hover? [hover? #f])
  (def ([h     (/ w phi)]
        [border-color (if hover? hover-color "black")]
        [border-w (if hover? 8 1)]
        [color (if selected? selected-color "white")]))
  (launder
   (cc-superimpose (filled-rectangle w h
                                     #:color        color
                                     #:border-color border-color
                                     #:border-width border-w)
                   (symbol-row (* h 0.75)
                               (card-number  card)
                               (card-symbol  card)
                               (card-shading card)
                               (card-color   card)))))

(def (table-w->card-w table-w)
  (/ table-w 5))

(def (card-table w cards [cols 4] [sep 10]
                 #:hover-idx      [hover-idx 0]
                 #:selected-cards [selected-cards '()])
  (def ([h      (/ w phi)]
        [card-w (table-w->card-w w)]))
  (cc-superimpose (filled-rectangle w h #:color "blue")
                  (table cols
                         (for/list ([i (in-range (length cards))]
                                    [c cards])
                           (card-pict card-w c
                                      #:hover?    (= i hover-idx)
                                      #:selected? (member i selected-cards)))
                         cc-superimpose
                         cc-superimpose
                         sep
                         sep)))

;; ; Returns top left (x. y) posn of the card table.
;; (def (tl-card-table game-pict)
;;   (def card-table (first (pict-children game-pict)))
;;   (cons (child-dx card-table)
;;         (child-dy card-table)))

(def frame (new frame%
                [label  "set!"]
                [width  frame-w]
                [height (inexact->exact (floor frame-h))]))

(def (handle-mouse-event event)
  (void))

(def (select-hover-idx!)
  (set-gs-selected-cards!
   game-state
   (if (member (gs-hover-idx game-state) (gs-selected-cards game-state))
       (remove (gs-hover-idx game-state)
               (gs-selected-cards game-state))
       (cons   (gs-hover-idx game-state)
               (gs-selected-cards game-state)))))

(def (handle-key-event canvas event)
  (case (send event get-key-code)
    ['right (when (< (add1 (gs-hover-idx game-state))
                     (gs-visible-card-count game-state))
              (set-gs-hover-idx! game-state
                                 (add1 (gs-hover-idx game-state))))]
    ['left (when (>= (sub1 (gs-hover-idx game-state)) 0)
             (set-gs-hover-idx! game-state
                                (sub1 (gs-hover-idx game-state))))]
    ['up (when (>= (- (gs-hover-idx game-state) 4)
                   0)
           (set-gs-hover-idx! game-state
                              (- (gs-hover-idx game-state) 4)))]
    ['down (when (< (+ (gs-hover-idx game-state) 4)
                    (gs-visible-card-count game-state))
             (set-gs-hover-idx! game-state
                                (+ (gs-hover-idx game-state) 4)))]
    ['#\return (select-hover-idx!)])
  (send canvas refresh))

(def (draw-game canvas dc)
  (send dc set-smoothing 'aligned)
  (draw-pict (card-table frame-w
                         (take (vector->list (gs-deck game-state))
                               (gs-visible-card-count game-state))
                         #:hover-idx (gs-hover-idx game-state)
                         #:selected-cards (gs-selected-cards game-state))
             dc 0 0))

(def game-canvas% (class canvas%
                    (define/override (on-event event)
                      (handle-mouse-event event))
                    (define/override (on-char event)
                      (handle-key-event this event))
                    (super-new)))

(def canvas (new game-canvas%
                 [parent         frame]
                 [paint-callback draw-game]))

(define (main)
  (send frame show #t))

(main)
