#lang racket
(require slideshow
         slideshow/play
         racket/splicing
         racket/stxparam
         (for-syntax syntax/parse
                     syntax/parse/experimental/template
                     racket/private/norm-define
                     racket/syntax
                     racket/list))
(provide define/staged run-stages stage)

(define-syntax-parameter stage
  (λ (stx) (raise-syntax-error #f "For use in the context of define/staged" stx)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience syntax for defining staged slides
(struct anim-info (which-stage skip-first? skip-last? steps delay name layout))
(struct staged-slide (stage->title×pict num-stages animation))
(define-syntax (define/staged stx)
  (syntax-parse stx
    [(_ header (~or (~once (~or (~seq #:num-stages num:expr) (~seq #:stages [stage-names:id ...])))
                    (~optional (~seq #:stage stage-id:id)
                               #:defaults ([stage-id (generate-temporary #'stage)]))
                    (~optional (~seq #:title title))
                    ;; TODO?: allow more than one animation in a staged fn?
                    (~optional (~seq #:anim-at anim-at:expr))
                    (~optional (~and #:skip-first skip-first))
                    (~optional (~and #:skip-last skip-last))
                    (~optional (~seq #:steps steps:expr))
                    (~optional (~seq #:delay delay:expr))
                    (~optional (~seq #:name name:expr))
                    (~optional (~seq #:layout layout:expr))) ...
                    body ...+)
     #:fail-unless (if (or (attribute skip-first)
                           (attribute skip-last)
                           (attribute steps)
                           (attribute delay))
                       (attribute anim-at)
                       #t)
     "Can only be given when an animation"
     (define num-stages* (if (attribute num)
                             #'num
                             (length (syntax->list #'(stage-names ...)))))
     (define/with-syntax num-stages num-stages*)
     (define/with-syntax the-anim
       ;; BUG 14170: can't nest ?? to get this to look nice.
       (if (attribute anim-at)
           (quasitemplate (anim-info anim-at
                                #,(syntax? (attribute skip-first))
                                #,(syntax? (attribute skip-last))
                                (?? steps 10)
                                (?? delay 0.05)
                                (?? name #f)
                                (?? layout 'auto)))
           #'#f))
     (define-values (id rhs)
       (normalize-definition
        (quasitemplate
         (define header
           (staged-slide
            (λ (stage-id)
               (syntax-parameterize ([stage (make-rename-transformer #'stage-id)])
                 (values (?? title #f) (let () body ...))))
            num-stages 
            the-anim)))
        #'lambda #t #f))
     (with-syntax ([num-stages num-stages*])
       (quasisyntax/loc stx 
         (splicing-let-values (#,@(if (attribute stage-names)
                                      #`([(stage-names ...) (values #,@(range num-stages*))])
                                      #'()))
           (define #,id #,rhs))))]))

(define/match (run-stages v #:stage [stage #f])
  [((staged-slide fn num anim) _)
   (define simple (λ (i)
                     (define-values (title pict) (fn i))
                     (slide #:title title pict)))
   (define do
     (match anim
       [(anim-info at-stage skip-first? skip-last? steps delay name layout)
        (λ (i)
           (cond [(= i at-stage)
                  (define-values (title pict-fn) (fn i))
                  (play pict-fn
                        #:steps steps
                        #:delay delay
                        #:name name
                        #:layout layout
                        #:skip-first? skip-first?
                        #:title title)
                  (unless skip-last? (slide (pict-fn 1.0)))]
                 [else (simple i)]))]
       [_ simple]))
   (if stage
       (do stage)
       (for ([i num]) (do i)))])