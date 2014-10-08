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

(struct slide-options (title name layout gap-size inset timeout condense?))
(define (title-only t)
  (slide-options t t 'auto (current-gap-size) (make-slide-inset 0 0 0 0) #f #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience syntax for defining staged slides
(struct anim-info (skip-first? skip-last? steps delay name layout title))
;; Groups are for running different chunks of the stages, not necessarily in linear order.
(struct staged-slide (stage->pict num-stages animation options top-options ctx groups))
(define-syntax (define/staged stx)
  (define-splicing-syntax-class stage-associated-info
    #:attributes (info)
    (pattern (~seq (~or (~optional (~seq #:name name:expr))
                        (~optional (~seq #:layout layout:expr))
                        (~optional (~seq #:title title:expr))
                        (~optional (~seq #:inset inset:expr))
                        (~optional (~seq #:gap-size gap-size:expr))
                        (~optional (~seq #:timeout timeout:expr))
                        (~optional (~seq #:condense? condense?:expr))) ...)
             #:attr info
             (template (slide-options (?? title #f)
                                      (?? name (?? title #f))
                                      (?? layout 'auto)
                                      (?? gap-size (current-gap-size))
                                      (?? inset (make-slide-inset 0 0 0 0))
                                      (?? timeout #f)
                                      (?? condense? (?? (and timeout #t) #f))))))
  (define-syntax-class stage-info
    #:attributes (ident info)
    (pattern ident:id #:attr info #f)
    (pattern [ident:id assoc:stage-associated-info]
             #:attr info (attribute assoc.info)))
  (define-splicing-syntax-class anim-info-cls
    #:attributes (s)
    (pattern (~seq (~or
                    (~optional (~and #:skip-first skip-first))
                    (~optional (~and #:skip-last skip-last))
                    (~optional (~seq #:steps steps:expr))
                    (~optional (~seq #:delay delay:expr))
                    (~optional (~seq #:name name:expr))
                    (~optional (~seq #:layout layout:expr))
                    (~optional (~seq #:title title:expr))) ...)
             #:attr s (quasitemplate (anim-info #,(syntax? (attribute skip-first))
                                       #,(syntax? (attribute skip-last))
                                       (?? steps 10)
                                       (?? delay 0.05)
                                       (?? name #f)
                                       (?? layout 'auto)
                                       (?? title #f)))))
  (syntax-parse stx
    [(_ header (~or (~once (~or (~seq #:num-stages num:expr)
                                (~seq #:stages [st:stage-info ...])))
                    (~optional (~seq #:contextualize ctx:expr))
                    (~optional (~seq #:stage stage-id:id)
                               #:defaults ([stage-id (generate-temporary #'stage)]))
                    (~once info:stage-associated-info)
                    ;; some number of animations.
                    (~seq #:anim-at
                          [anim-at:expr anim-args:anim-info-cls])
                    ;; some number of stage groups
                    (~seq #:group gname:id group-stages:expr)) ...
                                      body ...+)
     #:do [(define stage-dup (and (attribute st.ident)
                                  (check-duplicate-identifier (attribute st.ident))))
           (define group-dup (check-duplicate-identifier (attribute gname)))]
     #:fail-when stage-dup
     (format "Duplicate stage name: ~a" stage-dup)
     #:fail-when group-dup
     (format "Duplicate stage group name: ~a" group-dup)
     (define num-stages* (if (attribute num)
                             #'num
                             (length (syntax->list #'(st.ident ...)))))
     (define/with-syntax num-stages num-stages*)
     (define-syntax if-named
       (syntax-rules ()
         [(_ e) (if (attribute st.ident) (list e) '())]
         [(_ t e) (if (attribute st.ident) t e)]))
     (define/with-syntax ((the-anims ...) ...)
       (map list (attribute anim-at) (attribute anim-args.s)))
     (define/with-syntax ((the-infos ...) ...)
       (if-named
        (for/list ([id (in-list (attribute st.ident))]
                   [info (in-list (attribute st.info))]
                   #:when (syntax? info))
          (list id info))
        '()))
     (define-values (id rhs)
       (normalize-definition
        (quasitemplate
         (define header
           (staged-slide
            (λ (stage-id*)
               (define-values (stage-id #,@(if-named #'the-stage-name))
                 (if (symbol? stage-id*)
                     #,(if-named
                        #'(values (hash-ref the-name-indexes stage-id*) stage-id*)
                        #'(error 'run-stages "Staged slide has unnamed stages: ~a" stage-id*))
                     (values stage-id*
                             #,@(if-named
                                 ;; only give a name if within bounds
                                 #'(and (fixnum? stage-id*)
                                         (<= 0 stage-id*) (< stage-id* num-stages)
                                         (vector-ref the-stage-names stage-id*))))))
               (syntax-parameterize ([stage (make-rename-transformer #'stage-id)]
                                     [stage-name (make-rename-transformer #'the-stage-name)])
                 body ...))
            num-stages
            (hash-no-dups the-anims ... ...)
            (hash-no-dups the-infos ... ...)
            info.info
            (?? ctx #f)
            (hash (?@ 'gname group-stages) ...))))
        #'lambda #t #f))
     (quasisyntax/loc stx 
       (splicing-let-values
           #,(if-named
              #`([(st.ident ...) (values #,@(range num-stages*))]
                 [(the-stage-names) '#(st.ident ...)]
                 ;; for nicer lookup when giving stages in run-stages
                 [(the-name-indexes) (hash #,@(append*
                                               (for/list ([name (in-list (attribute st.ident))]
                                                          [i (in-naturals)])
                                                 (list #`(quote #,name) i))))])
              #'())
         (define #,id #,rhs)))]))

(define/match (run-stages v #:stage [stage #f] #:group [group #f])
  [((staged-slide fn num anims options top-options ctx groups) _ _)
   (when (and stage group)
     (error 'run-stages "Cannot give both stage and group to run."))
   (define (do i)
     (match (hash-ref anims i #f)
       [(anim-info skip-first? skip-last? steps delay name layout title)
        (define pict-fn (fn i))
        (define f (if ctx (λ (n) (ctx (λ () (pict-fn n)))) pict-fn))
        (define title* (or title
                           (and (slide-options? top-options)
                                (slide-options-title top-options))))
        (play f
              #:steps steps
              #:delay delay
              #:name name
              #:layout layout
              #:skip-first? skip-first?
              #:title title*)
        (unless skip-last? (slide #:title title* (pict-fn 1.0)))]
       [_ (define pict (if ctx (ctx (λ () (fn i))) (fn i)))
          (match-define
           (slide-options title name layout gap-size inset timeout condense?)
           (hash-ref options i top-options))
          (slide #:title title #:layout layout #:name name
                 #:gap-size gap-size #:inset inset #:timeout timeout #:condense? condense?
                 pict)]))
   (define (do-group name)
     (match (hash-ref groups name #f)
       [(? list? stages)
        (for-each do stages)]
       [_ (error 'run-stages "Unknown group: ~a" name)]))
   (cond [(or (real? stage) (symbol? stage))
          ;; XXX: could cause undesired errors, but out of bounds inputs
          ;; might be the user's desire.
          (do stage)]
         [(list? stage) (for-each do stage)]
         [(symbol? group) (do-group group)]
         [(list? group) (for-each do-group group)]
         [else (for ([i num]) (do i))])])
