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
;; Groups are for running different chunks of the stages, not necessarily in linear order.
(struct staged-slide (stage->pict num-stages animation options top-options names ctx groups))

(define stage/c (or/c exact-nonnegative-integer? symbol?))
(define static-anim/c (cons/c stage/c (real-in 0 1)))

(provide define/staged
         (contract-out
          [run-stages
           (->* ((or/c staged-slide? (-> void?)))
                (#:stage (or/c #f stage/c
                               static-anim/c
                               (listof (or/c stage/c static-anim/c)))
                  #:group (or/c #f symbol? (listof symbol?)))
                void?)])
         (struct-out staged-slide)
         (struct-out anim-info)
         (struct-out slide-options)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience syntax for defining staged slides
(struct anim-info (skip-first? skip-last? steps delay name layout title))

(struct nested-stage (ss start-stage group))

(begin-for-syntax
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
            #:attr info (attribute assoc.info))
   ;; treat the running of a different staged-slide as a single stage.
   (pattern [#:compose (~or
                        (~optional ident:id #:defaults
                                   ([ident (first (generate-temporaries #'(dummy)))]))
                        (~once staged:expr)
                        (~optional (~seq #:stage st:expr))
                        (~optional (~seq #:group grp:expr))) ...]
            #:attr info (template (nested-stage staged (?? st #f) (?? grp #f)))))

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
 )

(define-syntax (define/staged stx)
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
            #,(if-named #'the-name-indexes #'(hash))
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

(define (mk-title pict-or-proc stage)
  (if (procedure? pict-or-proc)
      (pict-or-proc stage)
      pict-or-proc))

(define (pict-with-options pict options stage)
  (match-define (slide-options title name layout gap-size inset timeout condense?) options)
  (slide #:title (mk-title title stage) #:layout layout #:name name
         #:gap-size gap-size #:inset inset #:timeout timeout #:condense? condense?
         pict))

(define/match (run-stages v #:stage [stage #f] #:group [group #f])
  [((staged-slide fn num anims options top-options names ctx groups) _ _)
   (when (and stage group)
     (error 'run-stages "Cannot give both stage and group to run."))
   (define (do i/pair)
     (define-values (i n)
       (match i/pair
         [(cons i n) (values i n)]
         [_ (values i/pair #f)]))
     (match (hash-ref anims (hash-ref names i i) #f)
       [(anim-info skip-first? skip-last? steps delay name layout title)
        (define pict-fn (fn i))
        (define f (if ctx (λ (n) (ctx (λ () (pict-fn n)))) pict-fn))
        (define title* (or (mk-title title i)
                           (and (slide-options? top-options)
                                (mk-title
                                 (slide-options-title top-options)
                                 i))))
        (if n
            (pict-with-options (f n)
                               (struct-copy slide-options top-options
                                            [title title*])
                               i)
            (begin
              (play f
                    #:steps steps
                    #:delay delay
                    #:name name
                    #:layout layout
                    #:skip-first? skip-first?
                    #:title title*)
              (unless skip-last? (slide #:title title* (pict-fn 1.0)))))]
       [_
        (define ind (hash-ref names i i))
        (define pict (if ctx (ctx (λ () (fn ind))) (fn ind)))
        (match (hash-ref options ind top-options)
          [(? slide-options? ops)
           (when (and n (not (procedure? pict)))
             (error 'run-stages "Given a stage,0-1 pair, but not an animation stage: ~a"
                    i/pair))
           (pict-with-options (if n (pict n) pict) ops ind)]
          [(nested-stage ss stage group)
           ;; nested-stage's stage overrides the requested stage's n argument.
           (run-stages ss #:stage stage #:group group)])]))
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
         [else (for ([i num]) (do i))])]
  ;; Not a staged-slide, but just a thunk that makes a slide.
  [((? procedure? p) _ _)
   (p)])
