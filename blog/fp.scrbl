#lang scribble/manual

@(require racket/sandbox
          net/url
          scribble/eval)
@(define ev
   (call-with-trusted-sandbox-configuration
     (lambda ()
       (parameterize ([sandbox-output 'string]
                      [sandbox-error-output 'string])
         (make-evaluator 'racket)))))

@interaction[
  #:eval ev
  (define zero (λ (f) (λ (x) x)))
]

@interaction[
  #:eval ev
  (define one (λ (f) (λ (x) (f x))))
]

@interaction[
  #:eval ev
  (define two (λ (f) (λ (x) (f (f x)))))
]

@interaction[
  #:eval ev
  (define succ (λ (n) (λ (f) (λ (x) (f (n f) x)))))
]

