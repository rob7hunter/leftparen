#lang scheme/base

;; form validation

(require "util.scm"
         "record.scm"
         "contract-lp.ss")

(provide validate
         ;; field-validate (via contract)
         )

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

;;
;; field-validate
;;
(provide/contract (field-validate (->* (symbol?)
                                       ((or/c #f (-> any/c any/c))
                                        #:msg-fn (-> any/c string?))
                                       (-> rec? (or/c #f string?)))))
;;
(define (field-validate field-name
                        (pred #f)
                        #:msg-fn (msg-fn (lambda (bad-val)
                                           (format "'~A' is an invalid value for field ~A."
                                                   field-name bad-val))))
  (lambda (rec)
    (aif (rec-prop rec field-name)
         (if pred
             (if (pred it)
                 #f
                 (msg-fn it))
             #f)
         (format "Missing field '~A'." field-name))))
