#lang racket
(require pict
         pict/code
         pict/flash
         unstable/gui/pict
         racket/draw
         (for-syntax syntax/parse)
         syntax/parse/define
         ;; For contracting improper lists:
         (only-in racket/contract/private/rand rand-choice)
         (only-in racket/contract/private/generate generate/direct)
         (only-in racket/contract/private/guts define/subexpression-pos-prop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copied from slideshow/play due to gui crap in documentation
(define (fail-gracefully t)
  (with-handlers ([exn:fail? (lambda (x) (values 0 0))])
    (t)))
(define (slide-pict base p p-from p-to n)
  (let-values ([(x1 y1) (fail-gracefully (lambda () (lt-find base p-from)))]
               [(x2 y2) (fail-gracefully (lambda () (lt-find base p-to)))])
    (pin-over base
              (+ x1 (* (- x2 x1) n))
              (+ y1 (* (- y2 y1) n))
              p)))
(define (fast-start n)
  (- 1 (* (- 1 n) (- 1 n))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define nonneg-real?
  (make-contract #:name 'nonnegative-real?
                 #:first-order (λ (x) (and (real? x) (>= x 0)))))
(define (within-width-and-height w h)
  (make-contract #:name (format "within width and height ~a ~a" w h)
                 #:first-order
                 (λ (rw)
                    (define 2v (* 2 rw))
                    (and (positive? (- w 2v))
                         (positive? (- h 2v))))))
(define (nondecreasing-listof c)
  (make-contract #:name `(nondecreasing-listof ,(contract-name c))
                 #:first-order
                 (λ (l) (and ((listof c) l) (apply <= l)))))

(define (list*of-generate elem-ctc)
  (λ (fuel)
     (define (mk-rand-list so-far)
       (rand-choice
         [1/5 so-far]
         [else (mk-rand-list (cons (generate/direct elem-ctc fuel) so-far))]))
     (mk-rand-list (if (= (random 5) 0) ;; improper with 20% probability
                       (generate/direct elem-ctc fuel)
                       '()))))

(define (for-each* f imp-lst)
  (match imp-lst
    ['() (void)]
    [(cons a d) (begin (f a) (for-each* f d))]
    [other (f other)]))

(define (map* f imp-lst)
  (match imp-lst
    ['() '()]
    [(cons a d) (cons (f a) (map* f d))]
    [other (f other)]))

(define-syntax (*-list*of stx)
  (syntax-parse stx
    [(_ predicate?:id name:expr generate:expr)
     (syntax
      (λ (input)
         (define ctc (coerce-contract 'name input))
         (define ctc-name (build-compound-type-name 'name ctc))
         (define proj (contract-projection ctc))
         (define (fo-check x)
           (and (predicate? x)
                (let loop ([x x])
                  (match x
                    ['() #t]
                    [(cons a d)
                     (and (contract-first-order-passes? ctc a)
                          (loop d))]
                    [other (contract-first-order-passes? ctc other)]))))
         (define ((ho-check check-all) blame)
           (let ([p-app (proj (blame-add-context blame "an element of"))])
             (λ (val)
                (unless (predicate? val)
                  (raise-blame-error blame val
                                     '(expected: "~s" given: "~e")
                                     'predicate? 
                                     val))
                (check-all p-app val))))
         (cond
          [(flat-contract? ctc)
           (make-flat-contract
            #:name ctc-name
            #:first-order fo-check
            #:projection (ho-check (λ (p v) (for-each* p v) v))
            #:generate (generate ctc))]
          [(chaperone-contract? ctc)
           (make-chaperone-contract
            #:name ctc-name
            #:first-order fo-check
            #:projection (ho-check map*)
            #:generate (generate ctc))]
          [else
           (make-contract
            #:name ctc-name
            #:first-order fo-check
            #:projection (ho-check map*))])))]))

(define list*of-func (*-list*of any/c list*of list*of-generate))
(define/subexpression-pos-prop (list*of x) (list*of-func x))

;; different constructors for coloring stuff
(provide/contract [list*of contract?]
                  [colorize-if (any/c pict? color/c . -> . pict?)]
                  [pin-over-center (pict? real? real? pict? . -> . pict?)]
                  [pin-over-vcenter (->* (pict? (or/c pict? real?) (or/c procedure? real?) pict?)
                                         [#:x-translate real?]
                                         pict?)]
                  [pin-over-hcenter (->* (pict? (or/c pict? real?) (or/c procedure? real?) pict?)
                                         [#:y-translate real?]
                                         pict?)]
                  [both ((boolean? . -> . void?) . -> . void?)]
                  [pin-under-all (pict? symbol? pict? . -> . pict?)]
                  [pin-over-tag (pict?
                                 (pict? pict-path? . -> . (values real? real?))
                                 symbol?
                                 (pict? . -> . pict?)
                                 . -> . pict?)]
                  [pin-under-tag (pict?
                                 (pict? pict-path? . -> . (values real? real?))
                                 symbol?
                                 (pict? . -> . pict?)
                                 . -> . pict?)]
                  ;; Better than ellipse/border
                  [thick-ellipse (->* (nonneg-real? nonneg-real?
                                       (real-in 0 255)
                                       color/c)
                                      (#:fill-color (or/c #f color/c))
                                      pict?)]
                  [thick-filled-rounded-rectangle
                   (->* (nonneg-real? nonneg-real?)
                        (real?
                         #:color color/c
                         #:style brush-style/c
                         #:angle real?
                         #:border-width (real-in 0 255)
                         #:border-color (or/c #f color/c)
                         #:border-style pen-style/c)
                        pict?)]
                  [mk-center (real? real? pict? pict? . -> . (values real? real?))]
                  [chop-at (real? real? real? . -> . real?)]
                  [chopped-interval-scale ((real-in 0 1) (real-in 0 1) . -> . ((real-in 0 1) . -> . (real-in 0 1)))]
                  [annulus
                   (->i ([w nonneg-real?]
                         [h nonneg-real?]
                         [rw (w h) (and/c nonneg-real? (within-width-and-height w h))])
                        [#:color [color (or/c #f color/c)]
                         #:style [style brush-style/c]
                         #:border-width [border-width (real-in 0 255)]
                         #:border-color [border-color (or/c #f color/c)]
                         #:border-style [border-style pen-style/c]]
                        [result pict?])]
                  [filled-rounded-rectangle-frame
                   (->* (pict?)
                        [#:color color/c
                         #:scale nonneg-real?
                         #:x-scale nonneg-real?
                         #:y-scale nonneg-real?
                         #:corner-radius real?
                         #:angle real?
                         #:border-width (real-in 0 255)
                         #:border-color color/c
                         #:border-style pen-style/c]
                        pict?)]
                  [filled-flash-frame
                   (->* (pict?)
                        [#:scale nonneg-real?
                         #:color (or/c color/c #f)
                         #:outline (or/c color/c #f)
                         #:n-points exact-positive-integer?
                         #:spike-fraction (real-in 0 1)
                         #:rotation real?]
                        pict?)]
                  [slide-and-compose (->* (pict? (vectorof pict?) pict?)
                                          [(pict? (real-in 0 1) . -> . pict?)]
                                          ((real-in 0 1) . -> . pict?))]
                  [play-n-at (exact-nonnegative-integer?
                              exact-nonnegative-integer?
                              (nondecreasing-listof exact-nonnegative-integer?)
                              (listof pict?)
                              boolean?
                              . -> . (listof pict?))]
                  [progressive-table
                   (->* (exact-nonnegative-integer?       ;; stage
                         (nondecreasing-listof exact-nonnegative-integer?) ;; stages
                         exact-nonnegative-integer? ;; ncols
                         (listof pict?)             ;; picts
                         (list*of (->* () #:rest (listof pict?) pict?)) ;; col-aligns
                         (list*of (->* () #:rest (listof pict?) pict?)) ;; row-aligns
                         (list*of real?) ;; col-seps
                         (list*of real?) ;; row-seps
                         )
                        [#:ghost? boolean?]
                        pict?)])

;; brush/pen not parameters, unfortunately.
;; Imperative save-restore to the "rescue."
(begin-for-syntax
 (define-syntax-class (gsr dc-stx)
   #:attributes (g s do)
   (pattern [g:id s:id (~optional (~seq #:if guard:expr)) r:expr ...]
            #:with do (if (attribute guard)
                          #`(unless guard (send #,dc-stx s r ...))
                          #`(send #,dc-stx s r ...)))))
(define-simple-macro (with-save dc (~var p (gsr #'dc)) body ...)
  (let* ([dcv dc]
         [v (send dcv p.g)])
    p.do
    body ...
    (send dcv p.s v)))
(define-syntax (with-save* stx)
  (syntax-parse stx
    [(_ dc () body ...) (syntax/loc stx (let () body ...))]
    [(_ dc (~and (give gives ...)
                 ((~var p (gsr #'dcv)) (~var ps (gsr #'dcv)) ...))
         body ...)
     (syntax/loc stx (let ([dcv dc])
                       (with-save dcv give
                                  (with-save* dcv (gives ...) body ...))))]))

;; ellipse/border does the wrong thing.
(define (thick-ellipse ew eh thickness color #:fill-color [fill-color #f])
  (define-values (fill-color* style)
    (if fill-color
        (values fill-color 'solid)
        (values "white" 'transparent)))
  (dc (λ (dc dx dy)
         (with-save* dc ([get-brush set-brush (send the-brush-list find-or-create-brush fill-color* style)]
                         [get-pen set-pen color thickness 'solid])
           (send dc draw-ellipse dx dy ew eh)))
      ew eh))

(define (annulus w h rw
                 #:color [color #f]
                 #:style [style 'solid]
                 #:border-color [border-color #f]
                 #:border-width [border-width 1]
                 #:border-style [border-style 'solid])
  (dc (lambda (dc x y)
        (define p (new dc-path%))
        (define w2 (/ w 2))
        (define h2 (/ h 2))
        (define 2rw (* 2 rw))
        
        (send p move-to (- w rw) h2)
        (send p arc rw rw (- w 2rw) (- h 2rw) 0 (* 2 pi))
        (send p move-to w h2)
        (send p arc 0 0 w h 0 (* 2 pi))
        (send p translate x y)
        (send p close)
        
        (define brush (if color
                          (send the-brush-list find-or-create-brush color style)
                          (send the-brush-list find-or-create-brush "white" 'transparent)))
        (define pen (if border-color
                        (send the-pen-list find-or-create-pen border-color border-width border-style)
                        (send the-pen-list find-or-create-pen "black" 1 'transparent)))
        (with-save* dc ([get-brush set-brush brush]
                        [get-pen set-pen pen])
          (send dc draw-path p)))
      w h))

(define (mk-center x y base top)
  (values (+ x (/ (pict-width base) 2)
             (- (/ (pict-width top) 2)))
          (+ y (/ (pict-height base) 2) (- (/ (pict-height top) 2)))))

(define (pin-under-all base tag pict)
  (define pw (pict-width pict))
  (define ph (pict-height pict))
  (define paths (find-tag* base tag))
  (for/fold ([pict* base]) ([path (in-set (list->set paths))]
                            #:unless (and (pair? path) (is-ghost? (car path))))
    (define-values (dx dy) (lt-find pict* path))
    (define p (first path))
    (define-values (dx* dy*) (mk-center dx dy p pict))
    (pin-under pict* dx* dy* pict)))

;; Pin the center of pict at dx dy offset from base's top left corner.
(define (pin-over-center base dx dy pict)
  (pin-over base
            (- dx (/ (pict-width pict) 2))
            (- dy (/ (pict-height pict) 2))
            pict))

(define (pin-over-vcenter base dx dy pict #:x-translate [x-translate 0])
  (define-values (x y)
    (if (procedure? dy)
        (dy base dx)
        (values dx dy)))
  (pin-over base
            (+ x x-translate)
            (- y (/ (pict-height pict) 2))
            pict))

(define (pin-over-hcenter base dx dy pict #:y-translate [y-translate 0])
  (define-values (x y)
    (if (procedure? dy)
        (dy base dx)
        (values dx dy)))
  (pin-over base
            (- x (/ (pict-width pict) 2))
            (+ y y-translate)
            pict))

(define (both f) (f #f) (f #t))

(define (is-ghost? pict)
  (match (pict-draw pict)
    [`(picture ,w ,h) #t] [_ #f]))

(define (chop-at min max i)
  (cond [(< i min) min]
        [(> i max) max]
        [else i]))
;; unit-interval ∈ [0, 1].
;; When unit-interval ∈ [min, max], uniformly scale from 0 to 1 as min approaches max.
;; When unit-interval < min. 0
;; When unit-interval > max. 1
(define (chopped-interval-scale min max)
  (define 1/distance (/ 1 (- max min)))
  (λ (unit-interval)
     (cond [(< unit-interval min) 0]
           [(> unit-interval max) 1]
           [else (* 1/distance (- unit-interval min))])))

;; All picts in pict-vec must be in base, as well as from-pic.
;; comp : pict? (real-in 0 1) → pict?
;; Animation sliding all picts in pict-vec to their places in base,
;; starting from from-pic. While sliding, the pic can be further transformed by comp.
(define (slide-and-compose base pict-vec from-pic [comp (λ (p n) p)])
  (define num (vector-length pict-vec))
  (λ (n)
     (for/fold ([p* base]) ([ipict (in-vector pict-vec)]
                            [i (in-naturals)])
       (define interval (chopped-interval-scale (/ i num) (min 1 (/ (add1 i) (max 1 (- num 2))))))
       (define windowed (interval n))
       (slide-pict p* (comp ipict windowed) from-pic ipict (fast-start windowed)))))

(define (colorize-if b p c) (if b (colorize p c) p))

(define (thick-filled-rounded-rectangle w h [corner-radius -0.25]
                                        #:color [color "black"]
                                        #:style [style 'solid]
                                        #:angle [angle 0]
                                        #:border-width [border-width 1]
                                        #:border-color [border-color #f]
                                        #:border-style [border-style 'solid])
    (let ([dc-path (new dc-path%)])
      (send dc-path rounded-rectangle 0 0 w h corner-radius)
      (send dc-path rotate angle)
      (let-values ([(x y w h) (send dc-path get-bounding-box)])
        (dc (λ (dc dx dy) 
              (with-save* dc ([get-brush set-brush
                                         (send the-brush-list find-or-create-brush color style)]
                              [get-pen set-pen #:if border-color border-color border-width border-style])
                (send dc draw-path dc-path (- dx x) (- dy y))))
            w h))))

(define (filled-rounded-rectangle-frame pict
                                        #:color [color "white"]
                                        #:scale [scale 1]
                                        #:x-scale [x-scale 1]
                                        #:y-scale [y-scale 1]
                                        #:corner-radius [corner-radius -0.25]
                                        #:angle [angle 0]
                                        #:border-width [border-width 1]
                                        #:border-color [border-color "black"]
                                        #:border-style [border-style 'solid])
  (define dx (* x-scale scale (pict-width pict)))
  (define dy (* y-scale scale (pict-height pict)))
  (define rect
    (thick-filled-rounded-rectangle dx dy
                                    corner-radius
                                    #:color color
                                    #:angle angle
                                    #:border-width border-width
                                    #:border-color border-color
                                    #:border-style border-style))
  (cc-superimpose rect pict))

(define (filled-flash-frame pict
                            #:scale [scale 3/2]
                            #:color [color #f]
                            #:outline [outline #f]
                            #:n-points [n-points 10]
                            #:spike-fraction [spike-fraction 0.25]
                            #:rotation [rotation 0])
  (define flash
    (filled-flash (* scale (pict-width pict)) (* scale (pict-height pict))
                  n-points spike-fraction rotation))

  (cc-superimpose
   (if outline
       (colorize (outline-flash (* scale (pict-width pict)) (* scale (pict-height pict))
                                n-points spike-fraction rotation)
                 outline)
       (blank))
   (colorize-if color flash color)
   pict))

;; n stage : natural
;; stages: monotonically non-decreasing list of naturals
;; lst: list
(define (play-n-at n stage stages lst ghost?)
  (let loop ([stages stages] [lst lst])
    (cond [(empty? stages)
           (if ghost?
               (map ghost lst)
               '())]
          [(<= (first stages) stage)
           (define-values (to-show the-rest) (split-at lst n))
           (append to-show (loop (rest stages) the-rest))]
          [else (loop (rest stages) lst)])))

;; Given the current stage and the "rollout" stages, show the rows
;; up to the current stage.
;; there should be as many stages as there are rows.
(define (progressive-table stage stages ncols picts col-aligns row-aligns col-seps row-seps #:ghost? [ghost? #t])
  (cond [(or (zero? ncols) (empty? stages) (empty? picts)) (blank)]
        [else
         (define rows (/ (length picts) (length stages)))
         (define prog-picts (play-n-at rows stage stages picts ghost?))
         (cond [(empty? prog-picts) (blank)]
               [else (table ncols prog-picts col-aligns row-aligns col-seps row-seps)])]))

(define (pin-at-tag pin base finder tag pict-fn)
  (define path (find-tag base tag))
  (pin base path finder (pict-fn (first path))))

(define (pin-under-tag base finder tag pict-fn)
  (pin-at-tag pin-under base finder tag pict-fn))
(define (pin-over-tag base finder tag pict-fn)
  (pin-at-tag pin-over base finder tag pict-fn))
