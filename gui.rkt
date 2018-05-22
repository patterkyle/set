#lang racket

(require (only-in racket/draw
                  brush%
                  color%
                  dc-path%
                  make-color
                  make-font
                  pen%)
         pict
         pict/convert
         racket/gui
         "prelude.rkt"
         "logic.rkt")

(def ([phi            (/ (+ 1 (sqrt 5)) 2)] ;φ, the golden ratio
      [frame-w        800]
      [frame-h        (/ frame-w phi)]
      [canvas-w       (* frame-w 0.75)]
      [pen-w          3]
      [hover-border-w 4]
      [bg-color       (make-color 0 0 255)]
      [hover-color    "fuchsia"]
      [selected-color "yellow"]
      [font-color     "white"]
      [game-font      (make-font #:size 14
                                 #:face "monospace")]
      [min-cards      4]
      [max-cards      20]))

;game state
(struct gs (status ;one of '(playing)
            deck
            vis-card-count ;number of cards to show on screen
            selected-cards ;indexes of cards the user has selected
            hover-idx ;currently highlighted card
            canvas-w
            canvas-h
            sets-found)
  #:mutable
  #:transparent)

(def state (gs 'playing         ;status
               (make-deck)      ;deck
               16               ;visible cards
               empty            ;selected cards
               0                ;hover card index
               canvas-w         ;canvas width
               (/ canvas-w phi) ;canvas height
               0))              ;sets found

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

(def (card-symbol->pict-fn symbol)
  (case symbol
    ['diamond  diamond]
    ['oval     oval]
    ['squiggle squiggle]))

(def (diamond-path w h)
  (def path (new dc-path%))
  (send path move-to (/ w 2) 0)
  (send path line-to w       (/ h 2))
  (send path line-to (/ w 2) h)
  (send path line-to 0       (/ h 2))
  (send path close)
  path)

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

; Defines a function named `id` that draws the given path.
; The returned function takes arguments (w h shading color) and returns a pict.
(define-syntax-rule (def-pict-from-path id path)
  (def (id w h shading color)
    (def ([pict-color   (card-color->pict-color     color)]
          [pict-shading (card-shading->pict-shading shading)]))
    (dc (λ (dc dx dy)
          (def ([old-brush (send dc get-brush)]
                [old-pen   (send dc get-pen)]))
          (send dc set-brush pict-color pict-shading)
          (send dc set-pen pict-color pen-w 'solid)
          (send dc draw-path (path w h) dx dy)
          (send dc set-brush old-brush)
          (send dc set-pen old-pen))
        w h)))

(def-pict-from-path diamond  diamond-path)
(def-pict-from-path oval     oval-path)
(def-pict-from-path squiggle squiggle-path)

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
                #:hover?    [hover? #f])
  (def ([h            (/ w phi)]
        [border-color (if hover? hover-color "black")]
        [border-w     (if hover? hover-border-w 1)]
        [color        (if selected? selected-color "white")]))
  (cc-superimpose (filled-rectangle w h
                                    #:color        color
                                    #:border-color border-color
                                    #:border-width border-w)
                  (symbol-row (* h 0.75)
                              (card-number  card)
                              (card-symbol  card)
                              (card-shading card)
                              (card-color   card))))

(def (card-table w cards [cols 4] [sep 10]
                 #:hover-idx      [hover-idx 0]
                 #:selected-cards [selected-cards '()])
  (def ([h      (/ w phi)]
        [card-w (/ w 4)]))
  (table cols
         (for/list ([i (in-range (length cards))]
                    [c cards])
           (card-pict card-w c
                      #:hover?    (= i hover-idx)
                      #:selected? (member i selected-cards)))
         cc-superimpose
         cc-superimpose
         sep
         sep))

(def frame (new frame%
                [label  "set!"]
                [width  frame-w]
                [height (inexact->exact (floor frame-h))]))

(def (select-hover-idx!)
  (def ([hover-idx (gs-hover-idx state)]
        [selected-cards (gs-selected-cards state)]))
  (set-gs-selected-cards! state
                          (if (member hover-idx selected-cards)
                              (remove hover-idx selected-cards)
                              (cons   hover-idx selected-cards))))

(def (hover-move-right!)
  (when (< (add1 (gs-hover-idx state))
           (gs-vis-card-count state))
    (set-gs-hover-idx! state
                       (add1 (gs-hover-idx state)))))

(def (hover-move-left!)
  (when (>= (sub1 (gs-hover-idx state)) 0)
    (set-gs-hover-idx! state
                       (sub1 (gs-hover-idx state)))))

(def (hover-mode-up!)
  (when (>= (- (gs-hover-idx state) 4) 0)
    (set-gs-hover-idx! state
                       (- (gs-hover-idx state) 4))))

(def (hover-move-down!)
  (when (< (+ (gs-hover-idx state) 4)
           (gs-vis-card-count state))
    (set-gs-hover-idx! state
                       (+ (gs-hover-idx state) 4))))

(def (get-selected-cards deck idx-lst)
  (for/list ([i idx-lst])
    (list-ref deck i)))

(def (set-found!)
  (def selected-cards (get-selected-cards (gs-deck state)
                                          (gs-selected-cards state)))
  (def new-deck (filter (λ (c)
                          (not (member c selected-cards)))
                        (gs-deck state)))
  (set-gs-sets-found! state
                      (add1 (gs-sets-found state)))
  (set-gs-deck! state new-deck)
  (set-gs-selected-cards! state empty))

(def (handle-enter-key!)
  (select-hover-idx!)
  (when (makes-set? (get-selected-cards (gs-deck state)
                                        (gs-selected-cards state)))
    (set-found!)))

(def (add-row!)
  (when (<= (+ (gs-vis-card-count state) 4) max-cards)
    (set-gs-vis-card-count! state
                            (+ (gs-vis-card-count state) 4))))

(def (remove-row!)
  (when (>= (- (gs-vis-card-count state) 4) min-cards)
    (set-gs-vis-card-count! state
                            (- (gs-vis-card-count state) 4))))

(def (handle-key-event canvas event)
  (case (send event get-key-code)
    ['right    (hover-move-right!)]
    ['left     (hover-move-left!)]
    ['up       (hover-mode-up!)]
    ['down     (hover-move-down!)]
    ['#\return (handle-enter-key!)]
    ['#\=      (add-row!)]
    ['#\-      (remove-row!)]
    ['#\q      (send frame show #f)] ;dev only
    #;['escape (exit)])              ;prod only
  (send canvas refresh))

(def (draw-game canvas dc)
  (send dc set-smoothing 'aligned)
  (send canvas set-canvas-background bg-color)

  (case (gs-status state)
    ['playing
     (def game-pict
       (card-table (/ (gs-canvas-w state) phi)
                   (take (gs-deck state)
                         (gs-vis-card-count state))
                   #:hover-idx (gs-hover-idx state)
                   #:selected-cards (gs-selected-cards state)))
     (draw-pict game-pict
                dc
                (- (/ (gs-canvas-w state) 2)
                   (/ (pict-width game-pict) 2))
                (- (/ (gs-canvas-h state) 2)
                   (+ 30 (/ (pict-height game-pict) 2))))
     (def text-pict (colorize (text
                               (string-append "Sets found: "
                                              (number->string (gs-sets-found
                                                               state)))
                               game-font)
                              font-color))
     (draw-pict text-pict
                dc
                (- (* (gs-canvas-w state) 0.5)
                   (/ (pict-width text-pict) 2))
                (* (gs-canvas-h state) 0.9))]))

(def game-canvas% (class canvas%
                    (define/override (on-char event)
                      (handle-key-event this event))
                    (define/override (on-size w h)
                      (set-gs-canvas-w! state w)
                      (set-gs-canvas-h! state h))
                    (super-new)))

(def canvas (new game-canvas%
                 [parent         frame]
                 [paint-callback draw-game]))

(def (main)
  (send frame show #t))

(main)
