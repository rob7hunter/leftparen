#lang scheme/base

(require "util.scm"
         (planet "javascript.ss" ("jaymccarthy" "javascript.plt" 1 2)))

(provide js-script-invoke
         js-array
         js-quote
         js-call
         js-call-on-load
         ;;Javascript s-expression syntax
         js
         javascript
         javascript-marshal
         javascript-unmarshal
         javascript->string
         string->javascriptx
         )

(define (js-script-invoke . js-strs)
  `(script ((language "javascript"))
           ,(string-join js-strs "\n")))

(define (js-array scheme-lst)
  (string-append "[" (string-join scheme-lst ", ") "]"))

(define (js-quote thing)
  (cond ((number? thing) (number->string thing))
        ((or (string? thing) (symbol? thing)) (format "'~A'" thing))
        ((eq? thing #t) "true")
        ((eq? thing #f) "false")
        (else (e "Don't know how to js-quote ~A." thing))))

(define (js-call-on-load fn-name #:quote-all (quote-all #f) . args)
  (format "$(document).ready(function() { ~A })"
          (apply js-call fn-name #:quote-all quote-all args)))

;;
;; js-call
;;
;; #:quote-all if #t, then assume all arguments are primitive JS values and js-quote them
;;
(define (js-call fn-name #:quote-all (quote-all #f) . args)
  (let ((args (if quote-all (map js-quote args) args)))
    (string-append fn-name "(" (string-join args ", ") ")")))
