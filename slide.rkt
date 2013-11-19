#lang racket
(require slideshow
         slideshow/play
         (only-in unstable/gui/slideshow stage stage-name)
         racket/splicing
         racket/stxparam
         (for-syntax syntax/parse
                     syntax/parse/experimental/template
                     racket/private/norm-define
                     racket/syntax
                     racket/list))
(provide define/staged run-stages
         ;; reprovide
         stage stage-name)

(define (hash-no-dups . kvs)
  (define-values (bad? bad)
    (let check ([keys (set)] [kvs kvs])
      (match kvs
        ['() (values #f #f)]
        [(list-rest k v kvs)
         (if (set-member? keys k)
             (values #t k)
             (check (set-add keys k) kvs))])))
  (when bad?
    (error 'run-stages "More than one #:anim-at for the same stage: ~a" bad))
  (apply hash kvs))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience syntax for defining staged slides
(struct anim-info (skip-first? skip-last? steps delay name layout))
(struct staged-slide (stage->title×pict num-stages animation))
(define-syntax (define/staged stx)
  (syntax-parse stx
    [(_ header (~or (~once (~or (~seq #:num-stages num:expr) (~seq #:stages [stage-names:id ...])))
                    (~optional (~seq #:stage stage-id:id)
                               #:defaults ([stage-id (generate-temporary #'stage)]))
                    (~optional (~seq #:title title))
                    ;; some number of animations.
                    (~seq #:anim-at
                          [anim-at:expr
                           (~or
                            (~optional (~and #:skip-first skip-first))
                            (~optional (~and #:skip-last skip-last))
                            (~optional (~seq #:steps steps:expr))
                            (~optional (~seq #:delay delay:expr))
                            (~optional (~seq #:name name:expr))
                            (~optional (~seq #:layout layout:expr))) ...])) ...
                                      body ...+)
     #:fail-unless (if (or (attribute skip-first)
                           (attribute skip-last)
                           (attribute steps)
                           (attribute delay))
                       (attribute anim-at)
                       #t)
     "Can only be given when an animation"
     #:do [(define dup (and (attribute stage-names)
                            (check-duplicate-identifier (attribute stage-names))))]
     #:fail-when dup
     (format "Duplicate stage name: ~a" dup)
     (define num-stages* (if (attribute num)
                             #'num
                             (length (syntax->list #'(stage-names ...)))))
     (define/with-syntax num-stages num-stages*)
     (define/with-syntax ((the-anims ...) ...)
       (for/list ([at (in-list (attribute anim-at))]
                  [-skip-first (in-list (attribute skip-first))]
                  [-skip-last (in-list (attribute skip-last))]
                  [-steps (in-list (attribute steps))]
                  [-delay (in-list (attribute delay))]
                  [-name (in-list (attribute name))]
                  [-layout (in-list (attribute layout))])
         (list at
               #`(anim-info
                  #,(syntax? -skip-first)
                  #,(syntax? -skip-last)
                  #,(or -steps #'10)
                  #,(or -delay #'0.05)
                  #,(or -name #'#f)
                  #,(or -layout #''auto)))))
     (define-values (id rhs)
       (normalize-definition
        (quasitemplate
         (define header
           (staged-slide
            (λ (stage-id)
               #,@(if (attribute stage-names)
                      #'((define the-stage-name (vector-ref the-stage-names stage-id)))
                      #'())
               (syntax-parameterize ([stage (make-rename-transformer #'stage-id)]
                                     [stage-name (make-rename-transformer #'the-stage-name)])
                 (values (?? title #f) (let () body ...))))
            num-stages
            (hash-no-dups the-anims ... ...))))
        #'lambda #t #f))
     (with-syntax ([num-stages num-stages*])
       (quasisyntax/loc stx 
         (splicing-let-values (#,@(if (attribute stage-names)
                                      #`([(stage-names ...) (values #,@(range num-stages*))]
                                         [(the-stage-names) '#(stage-names ...)])
                                      #'()))
           (define #,id #,rhs))))]))

(define/match (run-stages v #:stage [stage #f])
  [((staged-slide fn num anims) _)
   (define (do i)
     (match (hash-ref anims i #f)
       [(anim-info skip-first? skip-last? steps delay name layout)
        (define-values (title pict-fn) (fn i))
        (play pict-fn
              #:steps steps
              #:delay delay
              #:name name
              #:layout layout
              #:skip-first? skip-first?
              #:title title)
        (unless skip-last? (slide #:title title (pict-fn 1.0)))]
       [_ (define-values (title pict) (fn i))
          (slide #:title title pict)]))
   (cond [(number? stage) (do stage)]
         [(list? stage) (for-each do stage)]
         [else (for ([i num]) (do i))])])