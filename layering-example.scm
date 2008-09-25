#lang scheme/base

(require scheme/unit)

(provide foo bar)

;; we want to be able to extend and have original library calls be able to call the
;; extended version.

;; the trick is to use a recursive unit.  very cool.

(define-signature my-sig^ (foo bar))

(define-unit base-unit%
  (import (prefix from-future: my-sig^))
  (export my-sig^)

  (define (foo)
    (list "foo base" (from-future:bar)))

  (define (bar)
    "bar base")
  )

(define-unit extension-unit%
  (import (prefix base: my-sig^))
  (export my-sig^)

  (define foo base:foo)

  (define (bar)
    "bar extension")
  )

(define-compound-unit merged-unit%
  (import)
  (export RESULT)
  (link [((BASE : my-sig^)) base-unit% RESULT]
        [((RESULT : my-sig^)) extension-unit% BASE]))

(define-values/invoke-unit merged-unit% (import) (export my-sig^))
