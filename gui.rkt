#lang racket

(require (only-in racket/draw
                  brush%
                  color%
                  dc-path%
                  make-font
                  pen%)
         pict
         pict/convert
         racket/gui
         "prelude.rkt"
         "logic.rkt")

(def ([phi            (/ (+ 1 (sqrt 5)) 2)]
      [frame-w        800]
      [frame-h        (/ frame-w phi)]
      [canvas-w       (* frame-w 0.75)]
      [pen-w          3]
      [bg-color       "blue"]
      [hover-color    "fuchsia"]
      [selected-color "yellow"]
      [game-font      (make-font #:size 16
                                 #:face "monospace")]
      [font-color     "white"]))

;game state
(struct gs (status
            deck
            visible-card-count
            selected-cards
            hover-idx
            canvas-w
            canvas-h
            sets-found
            used-cards)
  #:mutable
  #:transparent)

(def game-state (gs 'playing         ;status
                    (make-deck)      ;deck
                    16               ;visible cards
                    empty            ;selected cards
                    0                ;hover card index
                    canvas-w         ;canvas width
                    (/ canvas-w phi) ;canvas height
                    0                ;sets found
                    empty))          ;used cards

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

(define-syntax-rule (defpict id path)
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

(defpict diamond  diamond-path)
(defpict oval     oval-path)
(defpict squiggle squiggle-path)

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
        [border-w     (if hover? 4 1)]
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

(def (handle-mouse-event event)
  (void))

(def (select-hover-idx!)
  (def ([hover-idx      (gs-hover-idx game-state)]
        [selected-cards (gs-selected-cards game-state)]))
  (set-gs-selected-cards!
   game-state
   (if (member hover-idx selected-cards)
       (remove hover-idx selected-cards)
       (cons   hover-idx selected-cards))))

(def (move-right!)
  (when (< (add1 (gs-hover-idx game-state))
           (gs-visible-card-count game-state))
    (set-gs-hover-idx! game-state
                       (add1 (gs-hover-idx game-state)))))

(def (move-left!)
  (when (>= (sub1 (gs-hover-idx game-state)) 0)
    (set-gs-hover-idx! game-state
                       (sub1 (gs-hover-idx game-state)))))

(def (move-up!)
  (when (>= (- (gs-hover-idx game-state) 4) 0)
    (set-gs-hover-idx! game-state
                       (- (gs-hover-idx game-state) 4))))

(def (move-down!)
  (when (< (+ (gs-hover-idx game-state) 4)
           (gs-visible-card-count game-state))
    (set-gs-hover-idx! game-state
                       (+ (gs-hover-idx game-state) 4))))

(def (get-selected-cards deck idx-lst)
  (for/list ([i idx-lst])
    (list-ref deck i)))

(def (set-found!)
  (def selected-cards (get-selected-cards (gs-deck game-state)
                                          (gs-selected-cards game-state)))
  (def new-deck (filter (λ (c)
                          (not (member c selected-cards)))
                        (gs-deck game-state)))
  (set-gs-sets-found! game-state
                      (add1 (gs-sets-found game-state)))
  (set-gs-deck! game-state new-deck)
  (set-gs-selected-cards! game-state empty))

(def (handle-enter-key!)
  (select-hover-idx!)
  (when (makes-set? (get-selected-cards (gs-deck game-state)
                                        (gs-selected-cards game-state)))
    (set-found!)))

(def (handle-key-event canvas event)
  (case (send event get-key-code)
    ['right    (move-right!)]
    ['left     (move-left!)]
    ['up       (move-up!)]
    ['down     (move-down!)]
    ['#\return (handle-enter-key!)]
    ['#\q      (send frame show #f)] ;dev only
    #;['escape (exit)]) ;prod only
  (send canvas refresh))

(def (draw-game canvas dc)
  (send dc set-smoothing 'aligned)
  (send canvas set-canvas-background (make-object color% 0 0 255))
  (def game-pict (card-table (/ (gs-canvas-w game-state) phi)
                             (take (gs-deck game-state)
                                   (gs-visible-card-count game-state))
                             #:hover-idx (gs-hover-idx game-state)
                             #:selected-cards (gs-selected-cards game-state)))
  (draw-pict game-pict
             dc
             (- (/ (gs-canvas-w game-state) 2)
                (/ (pict-width game-pict) 2))
             (- (/ (gs-canvas-h game-state) 2)
                (+ 30 (/ (pict-height game-pict) 2))))
  (def text-pict (colorize (text
                            (string-append "Sets found: "
                                           (number->string (gs-sets-found
                                                            game-state)))
                            game-font)
                           font-color))
  (draw-pict text-pict
             dc
             (- (* (gs-canvas-w game-state) 0.5)
                (/ (pict-width text-pict) 2))
             (* (gs-canvas-h game-state) 0.85)))

(def game-canvas% (class canvas%
                    (define/override (on-event event)
                      (handle-mouse-event event))
                    (define/override (on-char event)
                      (handle-key-event this event))
                    (define/override (on-size w h)
                      (set-gs-canvas-w! game-state w)
                      (set-gs-canvas-h! game-state h))
                    (super-new)))

(def canvas (new game-canvas%
                 [parent         frame]
                 [paint-callback draw-game]))

(def (main)
  (send frame show #t))

(main)
