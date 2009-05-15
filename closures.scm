#lang scheme/base

(require (file "util.scm")
         "web-export.ss"
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

(define-syntax body-as-closure-key-aux
  (syntax-rules ()
    ((_ req-iden key-iden is-sticky-expr body ...)
     (let ((is-sticky is-sticky-expr))
       (add-closure! #:key key-iden
                     (lambda (req-iden)
                       ;; first cleanup after itself (if not sticky)
                       (unless is-sticky (hash-remove! CLOSURES key-iden))
                       ;; then run the actual closure...
                       body ...))))))

(define-syntax body-as-closure-key
  (syntax-rules ()
    ((_ (req-iden #:sticky) body ...)
     (let ((key-iden (make-closure-key)))
       (body-as-closure-key-aux req-iden key-iden #t body ...)))
    ((_ (req-iden key-expr #:sticky) body ...)
     (let ((key-iden key-expr))
       (body-as-closure-key-aux req-iden key-iden #t body ...)))
    ((_ (req-iden) body ...)
     (let ((key-iden (make-closure-key)))
       (body-as-closure-key-aux req-iden key-iden #f body ...)))
    ((_ (req-iden key-expr) body ...)
     (let ((key-iden key-expr))
       (let ((key-iden key-expr))
         (body-as-closure-key-aux req-iden key-iden #f body ...))))))

;;
;; body-as-url
;;
;; Forms:
;; (body-as-url (req) body ...)
;; (body-as-url (req fn-key) body ...)
;; (body-as-url (req #:sticky) body ...)
;; (body-as-url (req fn-key #:sticky) body ...)
;;
;; If fn-key is given, it will be the key used to map to the body.
;; (This provides a way for the developer to reuse fns in certain situations.)
;; 
;; If #:sticky appears at the end of the first argument, then the closure will not
;; be removed form memory after it's invoked (on a server restart, however, all closures
;; --regardless of stickiness--are cleared).
;;
(define-syntax body-as-url
  (syntax-rules ()
    ((_ (spec ...) body ...)
     (closure-key->url (body-as-closure-key (spec ...) body ...)))))

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
