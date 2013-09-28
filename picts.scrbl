#lang scribble/manual
@(require scribble/eval 
          racket/sandbox
          (for-label scribble/struct
                     "picts.rkt"
                     racket/set
                     racket/base))
@(define main-eval
   (make-base-eval)
   #;
   (call-with-trusted-sandbox-configuration
    (λ ()
     (parameterize ([sandbox-output 'string]
                    [sandbox-error-output 'string])
       (make-evaluator 'racket/base #:requires (list 'racket/set "picts.rkt" 'pict/code 'pict))))))
@(main-eval '(require racket/set "picts.rkt" pict/code pict))

@title[#:tag "picts"]{Constructors and combinators for @racket[pict]}
@(declare-exporting slideshow-helpers/picts)
@defmodule*/no-declare[(slideshow-helpers/picts)]{}


This library provides some extra support for constructing and combining @racket[pict]s.
It originated as a helper library for several of my presentations, and since I've had some reuse with them, so might others.

@defproc[(list*of [contract contract?]) contract?]{The "contract" constructor that is alluded to by the documentation for @racket[table]. It is an improper list contract, where the last cons pair's @racket[cdr] may be @racket['()] or a value satisfying @racket[contract].

@examples[#:eval main-eval
(andmap (list*of number?) '(3 (0 1) (0 1 . 2) ()))
((list*of number?) '(0 . nope))]}

@defproc[(colorize-if [test any/c] [pict pict?] [color color/c]) pict?]{A useful pattern: @racket[(if test (colorize pict color) pict)]}

@defproc[(pin-over-center [base pict?] [x real?] [y real?] [pict pict?]) pict?]{Pin the center of @racket[pict] to @racket[x] and @racket[y] over @racket[base].}

@defproc[(pin-over-hcenter [base pict?] [x real?] [y real?] [pict pict?]) pict?]{Like @racket[pin-over-center], only centers the x-axis.}

@defproc[(pin-over-vcenter [base pict?] [x real?] [y real?] [pict pict?]) pict?]{Like @racket[pin-over-hcenter], but for the y-axis.}

@defproc[(both [fn (-> boolean? void?)]) void?]{To be used with @racket[slide]-producing functions that have only two modes, signified by @racket[#t] and @racket[#f].}

@defproc[(pin-under-all [base pict?] [tag symbol?] [pict pict?]) pict?]{Center @racket[pict] under all picts that are tagged (with @racket[tag-pict]) with @racket[tag] in @racket[base].}

@defproc[(pin-over-tag [base pict?] [finder (-> pict? pict-path? (values real? real?))] [tag symbol?] [wrap (-> pict? pict?)]) pict?]{Find a pict in @racket[base] tagged @racket[tag], apply @racket[wrap] to the found pict and pin over @racket[base] at the coordinates given by @racket[finder].}

@defproc[(pin-under-tag [base pict?] [finder (-> pict? pict-path? (values real? real?))] [tag symbol?] [wrap (-> pict? pict?)]) pict?]{Like @racket[pin-over-tag], but uses @racket[pin-under].}

@defproc[(thick-ellipse [w nonneg-real?] [h nonneg-real?] [border-width (real-in 0 255)] [color color/c] [#:fill-color fill-color (or/c #f color/c) #f]) pict?]{Like @racket[ellipse/border], only uses the pen to draw a border rather than layer different colored ellipses. This produces more consistent borders.}

@defproc[(thick-filled-rounded-rectangle [w nonneg-real?]
                                         [h nonneg-real?]
                                         [corner-radius real? -0.25]
                                         [#:color color color/c "black"]
                                         [#:style style brush-style/c 'solid]
                                         [#:angle angle real? 0]
                                         [#:border-width border-width (real-in 0 255) 1]
                                         [#:border-color border-color (or/c #f color/c) #f]
                                         [#:border-style border-style pen-style/c]) pict?]{
Like @racket[filled-rounded-rectangle], but adds a border with a pen. Can additionally rotate the rectangle by @racket[angle].}

@defproc[(filled-rounded-rectangle-frame
           [pict pict?]
           [#:corner-radius corner-radius real? -0.25]
           [#:scale scale nonneg-real? 1]
           [#:x-scale x-scale nonneg-real? 1]
           [#:y-scale y-scale nonneg-real? 1]
           [#:color color color/c "white"]
           [#:angle angle real? 0]
           [#:border-width border-width (real-in 0 255) 1] 
           [#:border-color border-color (or/c #f color/c) #f]
           [#:border-style border-style pen-style/c])
          pict?]{
Uses @racket[thick-filled-rounded-rectangle] to form a frame around a given pict, and gives the ability to scale uniformly and with each dimension. The x-axis and y-axis are scaled by @racket[(* scale x-axis)] and @racket[(* scale y-axis)] respectively.}

@defproc[(filled-flash-frame [pict pict?] [#:scale scale nonneg-real? 1]
                             [#:corner-radius corner-radius real? -0.25]
                             [#:outline outline (or/c #f color/c) #f]
                             [#:n-points n-points exact-positive-integer? 10]
                             [#:spike-fraction spike-fraction (real-in 0 1) 1]
                             [#:rotation rotation real? 0])
         pict?]{
Use @racketmodname[pict/flash] to produce a frame around a given pict. If @racket[outline] not @racket[#f], then additionally draws an outlined flash with the color bound to @racket[outline].}

@defproc[(play-n-at [n exact-nonnegative-integer?]
                    [stage exact-nonnegative-integer?]
                    [stages (nondecreasing-listof exact-nonnegative-integer?)]
                    [picts (listof pict?)]
                    [ghost-rest? boolean?])
         (listof pict?)]{
Chunks @racket[picts] into strides of @racket[n], for each number in @racket[stages]. If @racket[stage] is less than or equal to a stage given in @racket[stages], then that stride is present in the output. If not, either stop and produce an empty tail, or ghost the rest of the picts in the tail, depending on @racket[ghost-rest?].}

@defproc[(progressive-table [stage exact-nonnegative-integer?]
                            [stages (nondecreasing-listof exact-nonnegative-integer?)]
                            [ncols exact-nonnegative-integer?]
                            [picts (listof pict?)]
                            [col-aligns (list*of (pict? pict? . -> . pict?))]
                            [row-aligns (list*of (pict? pict? . -> . pict?))]
                            [col-seps (list*of real?)]
                            [row-seps (list*of real?)]
                            [#:ghost? ghost? boolean? #t])
         pict?]{
An interface for staging rows in a @racket[table], that uses @racket[play-n-at] to produce the picts for @racket[table].

@examples[#:eval main-eval
(define (foo stage)
 (define (angles p) (hc-append (text "〈") p (text "〉")))
 (progressive-table stage (list 0 0 1 2) 2
                    (list (angles (code x ρ₁ σ₂)) (code 1)
                          (angles (code (f y) ρ₁ σ₁)) (code 1)
                          (angles (code x ρ₅ σ₅)) (code 2)
                          (angles (code (f y) ρ₄ σ₄)) (code 2))
                    lc-superimpose cc-superimpose 24 5))
(foo 0)
(foo 1)
(foo 2)]
}

@close-eval[main-eval]
