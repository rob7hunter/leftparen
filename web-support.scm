#lang scheme/base

(require (file "util.scm")
         (lib "xml.ss" "xml")
         net/url
         scheme/serialize
         (rename-in scheme/contract
                    (any c:any))
         (planet "web.scm" ("soegaard" "web.plt" 2 1))
         )

(provide request-all-bindings
         final-prep-of-response
         xexpr->de-grouped-xexprs
         wrap-each-in-list
         wrap-each-in-list-with-attrs
         redirect-to
         web-link
         img
         raw-str
         with-binding ; from web.plt
         basic-response
         xexpr-if
         url+query
         url->string
         get-url
         bindings/string
         find-binding
         )

;;
;; list-response
;;
(define (any-responses? lst)
  (any (lambda (elt)
         (or (response/full? elt) (response/incremental? elt) (response/basic? elt)))
       lst))
(provide/contract
 (list-response (->*
                 ;; required
                 ((not/c any-responses?))
                 ;; optional
                 (#:type bytes? #:extras list?)
                 ;; returns
                 response?)))
;;
(define (list-response content-lst #:type (type #"text/html") #:extras (extras '()))
  (basic-response (append-map (lambda (content) (map xexpr->string
                                                     (xexpr->de-grouped-xexprs content)))
                              content-lst)
                  #:type type
                  #:extras extras))

(define (basic-response content-lst #:type (type #"text/html") #:extras (extras '()))
  ;; right now we always no-cache.  we'll probably eventually want something more
  ;; subtle.
  (let ((no-cache (make-header #"Cache-Control" (string->bytes/utf-8 "no-cache;"))))
    (make-response/full 200 "all good" (current-seconds)
                        type (cons no-cache extras)
                        content-lst)))

(define-serializable-struct binding/string (id))
(define-serializable-struct (binding/string:form binding/string) (value))
(define-serializable-struct (binding/string:file binding/string) (filename content))
(provide/contract
 [struct binding/string ([id string?])]
 [struct (binding/string:form binding/string) ([id string?]
                                               [value string?])]
 [struct (binding/string:file binding/string) ([id string?]
                                                [filename string?]
                                                [content bytes?])])

(define (bindings/string req [localization-function bytes->string/utf-8])
  (map (lambda (binding)
         (if (binding:form? binding)
           (make-binding/string:form (localization-function (binding-id binding))
                                     (localization-function (binding:form-value binding)))
           (make-binding/string:file (localization-function (binding-id binding))
                                     (localization-function (binding:file-filename binding))
                                     (binding:file-content binding))))
       (request-bindings/raw req)))

(define (find-binding field bindings)
; strait from request-struct.ss
  (match bindings
    [(list)
     #f]
    [(list-rest (and b (struct binding/string (i))) bindings)
     (if (string=? field i)
         b
         (find-binding field bindings))]))

;; if you are doing a post, this gives you post and get vars.  if a get, it's just reg.
(define (request-all-bindings req)
  (append (request-bindings req)
          (if (request-post-data/raw req) ; there a better way to check if it's a post?
              (url-query (request-uri req))
              '())))

(define (group-tag? xexpr)
  (match xexpr ((list-rest 'group children) #t) (else #f)))

(define (final-prep-of-response xexpr-or-response)
  (let ((result (xexpr->de-grouped-xexprs xexpr-or-response)))
    (if (and (length= result 1) (response? (first result)))
        (first result)
        (list-response result))))

(define (xexpr->de-grouped-xexprs xexpr)
  (cond ((not xexpr) '())
        ((not (list? xexpr)) (list xexpr))
        ((group-tag? xexpr) (append-map xexpr->de-grouped-xexprs (rest xexpr)))
          (else (receive (tag attrs children) (xexpr->tag*attrs*children xexpr)
                  (list (create-xexpr tag attrs
                                      (append-map xexpr->de-grouped-xexprs children)))))))

(define (attrs? thing)
  (and (list? thing)
       (or (empty? thing) (not (symbol? (first thing))))))

(define (create-xexpr tag attrs children)
  (if (empty? attrs)
      `(,tag ,@children)
      `(,tag ,attrs ,@children)))

(define (xexpr->tag*attrs*children xexpr)
    (let ((tag (first xexpr))
          (but-tag (rest xexpr)))
      (if (empty? but-tag)
          (values tag '() '())
          (let ((next (first but-tag)))
            (if (attrs? next)
                (values tag next (rest but-tag))
                (values tag '() but-tag))))))

;; the wrap-each-in* fns filter out #f values from elts:  
(define (wrap-each-in-list tag elts)
  (filter-map (lambda (e) (and e `(,tag ,e))) elts))

(define (wrap-each-in-list-with-attrs tag attrs elts)
  (filter-map (lambda (e) (and e `(,tag ,attrs ,e))) elts))

(define (web-link label url #:class (class #f) #:extra-attrs (extra-attrs '()))
  `(a ((href ,(if (string? url) url (url->string url)))
       ,@(append (if class `((class ,class)) '()) extra-attrs))
      ,label))

;; image-file is relative to /i/
(define (img image-file #:class (class #f))
  `(img ((src ,(string-append "/i/" image-file)) (border "0")
         ,@(splice-if class `(class ,class)))))

(define (raw-str str)
  (make-cdata #f #f str))

;;
;; xexpr-if
;;
;; Use if you only want to create an xexpr if a condition is true.  E.g.,
;;  (ul (li "Item 1") ,(xexpr-if (= 2 2) `(li "Item 2")))
;;
(define-syntax xexpr-if
  (syntax-rules ()
    ((_ test)
     (or test ""))
    ((_ test body ...)
     (if test (begin body ...) ""))))

;;
;; url+query
;;
;; query-alist is ((key . str) ...)
;; given query strs should *not* be url encoded (this will be done by url+query).
;;
(define (url+query url-str query-alist)
  (let ((tmp-url (string->url url-str)))
    (make-url (url-scheme tmp-url)
              (url-user tmp-url)
              (url-host tmp-url)
              (url-port tmp-url)
              (url-path-absolute? tmp-url)
              (url-path tmp-url)
              (append (url-query tmp-url) query-alist)
              (url-fragment tmp-url))))

;;
;; get-url
;;
;; exn-handler: exn -> any
;;
(define (get-url url port-handler #:exn-handler (exn-handler #f))
  (let ((thunk (lambda () (call/input-url (if (url? url) url (string->url url))
                                          get-pure-port
                                          port-handler))))

    (if exn-handler
        (with-handlers ((exn:fail:network? exn-handler)) (thunk))
        (thunk))))
