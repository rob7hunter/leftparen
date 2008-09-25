#lang scheme/base

;; form validation

(require "util.scm"
         "record.scm")

(provide validate field-validate)

;; constructs a fn suitable for passing in to the #:validate keyword of a form call
;; the fn : rec -> content
;;
;; Usage ex: (validate (field-validate 'name string?)
;;                     (field-validate 'age (lambda (n) (and (integer? n) (>= n 13)))))
;;
(define (validate . validation-fns)
  (lambda (rec)
    (let ((errors (filter-map (lambda (f) (f rec)) validation-fns)))
      (if (empty? errors)
          #f
          (string-join errors "\n")))))

(define (field-validate field-name (pred #f))
  (lambda (rec)
    (aif (rec-prop rec field-name)
         (if pred
             (if (pred it)
                 #f
                 (format "Validation error triggered by failure of ~A on field '~A'."
                         pred field-name))
             #f)
         (format "Missing field '~A'." field-name))))
