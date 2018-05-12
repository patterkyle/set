;; #lang racket

;; (require racket/draw
;;          racket/gui
;;          "prelude.rkt"
;;          "logic.rkt")

;; (define ([game-name        "Set."]
;;       [frame-w          800]
;;       [frame-h          500]
;;       [row-count        4]
;;       [col-count        4]
;;       [card-w           (/ frame-w col-count)]
;;       [card-h           (/ frame-h row-count)]
;;       [shape-w          (/ card-w 4)]
;;       [shape-h          (* card-h 0.8)]
;;       [shape-y-offset   (/ (- card-h shape-h) 2)]
;;       [card-bg-color    (make-color 242 242 242)]
;;       [card-bg-hl-color (make-color 254 250 138)]
;;       [success-msg      "HEY, YOU FOUND A SET!!!"]
;;       [game-over-msg    "WHEW. THAT WAS FUN."]
;;       [game-status      'playing]
;;       [reset-interval   2500] ; milliseconds
;;       [shown-card-count 16]))

;; (define ([selected-cards '()]
;;       [deck           (make-deck)]))

;; (define (diamond-path w h)
;;   (define path (new dc-path%))
;;   (send path move-to
;;         (/ w 2)      0)
;;   (send path line-to
;;         w            (/ h 2))
;;   (send path line-to
;;         (/ w 2)      h)
;;   (send path line-to
;;         0            (/ h 2))
;;   (send path close)
;;   path)

;; (define (oval-path w h)
;;   (define ([path    (new dc-path%)]
;;         [curve-h (/ h 8)]))
;;   (send path move-to
;;         0             curve-h)
;;   (send path curve-to
;;         (/ w 3)       (- (/ curve-h 5))
;;         (* w 2/3)     (- (/ curve-h 5))
;;         w             curve-h)
;;   (send path line-to
;;         w             (- h curve-h))
;;   (send path curve-to
;;         (* w 2/3)     (+ h (/ curve-h 5))
;;         (* w 1/3)     (+ h (/ curve-h 5))
;;         0             (- h curve-h))
;;   (send path close)
;;   path)

;; (define (squiggle-path w h)
;;   (define ([path   (new dc-path%)]
;;         [offset (/ h 64)]))
;;   (send path move-to
;;         0             offset)
;;   (send path curve-to
;;         (/ w 4)       (+ (/ offset 2))
;;         (* w 3/4)     (+ (/ offset 2))
;;         w             (/ h 4))
;;   (send path curve-to
;;         (/ w 2)       (/ h 2)
;;         (* w 3/4)     (* h 3/4)
;;         w             (- h offset))
;;   (send path curve-to
;;         (* w 3/4)     (- h (/ offset 2))
;;         (/ w 4)       (- h (/ offset 2))
;;         0             (* h 3/4))
;;   (send path curve-to
;;         (/ w 2)       (/ h 2)
;;         (/ w 4)       (/ h 4)
;;         0             offset)
;;   (send path close)
;;   path)

;; (define (shape-coords number card-w shape-w y)
;;   (for/list ([x (map (λ (n)
;;                        (- n (/ shape-w 2)))
;;                      (case number
;;                        ['one   (list (* card-w 1/2))]
;;                        ['two   (list (* card-w 1/3)
;;                                      (* card-w 2/3))]
;;                        ['three (list (* card-w 1/5)
;;                                      (* card-w 1/2)
;;                                      (* card-w 4/5))]))])
;;     (cons x y)))

;; (define (draw-card dc card x y w h [selected? #f])
;;   (defineine-values (color shading path)
;;     (values (case (card-color card)
;;               ['red    "red"]
;;               ['green  "green"]
;;               ['purple "purple"])
;;             (case (card-shading card)
;;               ['solid   'solid]
;;               ['striped 'horizontal-hatch]
;;               ['open    'transparent])
;;             (case (card-symbol card)
;;               ['diamond  diamond-path]
;;               ['oval     oval-path]
;;               ['squiggle squiggle-path])))

;;   (send dc set-pen "black" 1 'solid)
;;   (send dc set-brush (if selected?
;;                          card-bg-hl-color
;;                          card-bg-color)
;;         'solid)
;;   (send dc draw-rectangle x y w h)

;;   (send dc set-brush color shading)
;;   (send dc set-pen color 3 'solid)

;;   (for ([coord (shape-coords (card-number card) card-w
;;                              shape-w shape-y-offset)])
;;     (match-let ([(cons cx cy) coord])
;;       (send dc draw-path (path shape-w shape-h) (+ x cx) (+ y cy)))))

;; (define (card-coords card-count rows cols card-w card-h)
;;   (for/list ([i (in-range card-count)])
;;     (let ([x (* (modulo i cols)
;;                 card-w)]
;;           [y (* (quotient i rows)
;;                 card-h)])
;;       (cons x y))))

;; (define (coord->card-index x y card-w card-h)
;;   (match (cons (quotient x card-w)
;;                (quotient y card-h))
;;     [(cons 0 0) 0]
;;     [(cons 1 0) 1]
;;     [(cons 2 0) 2]
;;     [(cons 3 0) 3]
;;     [(cons 0 1) 4]
;;     [(cons 1 1) 5]
;;     [(cons 2 1) 6]
;;     [(cons 3 1) 7]
;;     [(cons 0 2) 8]
;;     [(cons 1 2) 9]
;;     [(cons 2 2) 10]
;;     [(cons 3 2) 11]
;;     [(cons 0 3) 12]
;;     [(cons 1 3) 13]
;;     [(cons 2 3) 14]
;;     [(cons 3 3) 15]))

;; (define (draw-cards dc cards x y w h)
;;   (for ([i     (in-range (length cards))]
;;         [card  cards]
;;         [coord (card-coords (length cards)
;;                             row-count col-count
;;                             card-w card-h)])
;;     (match-let ([(cons x y) coord])
;;       (draw-card dc card x y card-w card-h (member i selected-cards)))))

;; (define (indexes->cards deck idx-lst)
;;   (for/list ([i idx-lst])
;;     (list-ref deck i)))

;; (define (deal-next-hand!)
;;   (set! deck (remove* (indexes->cards deck selected-cards)
;;                       deck))
;;   (set! game-status 'playing)
;;   (set! selected-cards '()))

;; (define (draw-game canvas dc)
;;   (send dc set-smoothing 'aligned)
;;   (match game-status
;;     ['playing   (draw-cards dc (take deck shown-card-count)
;;                             0 0 frame-w frame-h)]
;;     ['set-found (begin
;;                   (send dc draw-text success-msg 100 100)
;;                   (new timer%
;;                        [notify-callback
;;                         (λ ()
;;                           (if (< (length deck) shown-card-count)
;;                               (set! game-status 'game-over)
;;                               (deal-next-hand!))
;;                           (send canvas refresh))]
;;                        [interval   reset-interval]
;;                        [just-once? #t]))]
;;     ['game-over (send dc draw-text game-over-msg 100 100)]))

;; (define (handle-event event canvas)
;;   (when (equal? (send event get-event-type)
;;                 'left-down)
;;     (define card-idx (coord->card-index (send event get-x)
;;                                      (send event get-y)
;;                                      card-w card-h))
;;     (set! selected-cards (if (member card-idx selected-cards)
;;                              (remove card-idx selected-cards)
;;                              (cons   card-idx selected-cards)))
;;     (when (makes-set? (indexes->cards deck
;;                                      selected-cards))
;;       (set! game-status 'set-found))
;;     (send canvas refresh)))

;; (define ([frame        (new frame%
;;                          [label  game-name]
;;                          [width  frame-w]
;;                          [height frame-h])]
;;       [panel        (new vertical-panel%
;;                          [parent frame])]
;;       [game-canvas% (class canvas%
;;                       (defineine/override (on-event event)
;;                         (handle-event event this))
;;                       (super-new))]
;;       [canvas       (new game-canvas%
;;                          [parent         panel]
;;                          [paint-callback draw-game])]))

;; (define (main)
;;   (send frame show #t))

;; (main)
