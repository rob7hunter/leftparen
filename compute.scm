#lang scheme/base

;; record computations

(require "record.scm"
         "util.scm")

(provide
 sum-recs
 )

(define (sum-recs recs prop-to-sum)
  (foldl (lambda (rec acc)
           (+ acc (rec-prop rec prop-to-sum)))
         0
         recs))
