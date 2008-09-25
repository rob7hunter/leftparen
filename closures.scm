#lang scheme/base

(require (file "util.scm")
         (planet "web.scm" ("soegaard" "web.plt" 2 1))
         (file "web-support.scm")
         (file "settings.scm"))

(provide add-closure!
         call-closure
         closure-key->url
         make-closure-key
         body-as-closure-key
         body-as-url
         handle-closure-in-req
         num-closures-in-memory
         )

(define-syntax body-as-closure-key
  (syntax-rules ()
    ((_ (req-identifier) body ...)
     (add-closure! (lambda (req-identifier) body ...)))
    ((_ (req-identifier key-identifier) body ...)
     (let ((key-identifier (make-closure-key)))
       (add-closure! #:key key-identifier
                     (lambda (req-identifier) body ...))))))

;;
;; body-as-url
;;
;; (body-as-url (req) body ...)
;; or
;; (body-as-url (req fn-key) body ...)
;; In the latter form, fn-key is the key that will be used to map to body.
;; This provides a way for the developer to reuse fns in certain situations.
;;
(define-syntax body-as-url
  (syntax-rules ()
    ((_ (identifiers ...) body ...)
     (closure-key->url (body-as-closure-key (identifiers ...) body ...)))))

(define (closure-key->url clos-key)
  (format "~A?~A=~A"
          (setting *WEB_APP_URL*)
          (setting *CLOSURE_URL_KEY*)
          clos-key))

(define-syntax handle-closure-in-req
  (syntax-rules ()
    ((_ req no-closure-body ...)
     (let ((url-key (setting *CLOSURE_URL_KEY*))
           (binds (request-bindings req)))
       (if (exists-binding? url-key binds)
           (call-closure (extract-binding/single url-key binds) req)
           (begin no-closure-body ...))))))

(define CLOSURES (make-hash))

(define (make-closure-key)
  (random-key-string 20))

;; returns the key
(define (add-closure! clos #:key (key #f))
  (let ((key (or key (make-closure-key))))
    (hash-set! CLOSURES key clos)
    key))

(define (call-closure key req)
  ((hash-ref CLOSURES key (lambda ()
                            (lambda (req)
                              (format "Expired or missing function '~A'." key))))
   req))

(define (num-closures-in-memory)
  (hash-count CLOSURES))
