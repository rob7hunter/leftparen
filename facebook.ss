#lang scheme/base

;; A PLT Scheme interface to the Facebook API.
;; NOT READY FOR USE YET!  Just a preliminary checkin...

(require (file "settings.scm")
         (file "util.scm")
         "contract-lp.ss"
         (file "web-support.scm")
         "page.scm"
         "form.scm"
         "util.scm"
         "js.scm"
         (planet "web.scm" ("soegaard" "web.plt" 2 (= 1)))
         web-server/private/request-structs ; should be ... web-server/http/request-structs
         (only-in (planet "json.ss" ("dherman" "json.plt" 1 (= 1))) (read read-json)))

(provide facebook-fn
         define-facebook-required-login-page
         facebook-require-login
         facebook-form
         facebook-strict-error
         ;; facebook-create-object (via contract)
         ;; facebook-complex-val (via contract)
         ;; facebook-session-key (via contract)
         ;; facebook-error (via contract)
         )

;; MMM this design prevents us from running multiple Facebook apps off the same
;; web server.
(declare-setting *FB_API_KEY*)
(declare-setting *FB_SECRET_KEY*)
(declare-setting *FB_API_VERSION* "1.0")
(declare-setting *FB_API_URL* "http://api.facebook.com/restserver.php")

;; API notes:  All API calls must have a method, api_key and sig parameter.  Other
;; parameters are optional or required depending on the particular method.  I think "v"
;; (api version) is always required too.

(define-syntax facebook
  (syntax-rules ()
     ((_ method)
      'foo)
     ((_ method keyword val rst ...)
      (keyword->string 'keyword))))

;;
;; if val-for-key is non-#f, it should be a symbol corresponding to a JSON hash table
;; key.  This function will return #f if the JSON result is not a hash, or does not
;; contain a value for that key.  When val-for-key is #f, we simply return the
;; generated JSON object.
;;
(define (facebook-fn method-str (bindings '())
                     #:val-for-key (val-for-key #f)
                     #:pass-session-from-req (req #f))
  (let* ((sys-bindings `((api_key . ,(setting *FB_API_KEY*))
                         (call_id . ,(number->string (current-milliseconds)))
                         (method . ,(string-append "facebook." method-str))
                         (format . "JSON")
                         (v . ,(setting *FB_API_VERSION*))))
         (augmented-sys-bindings (if req
                                     (alist-cons 'session_key (facebook-session-key req)
                                                 sys-bindings)
                                     sys-bindings))
         (sans-sig (sort (append bindings augmented-sys-bindings)
                         (match-lambda* ((list (list-rest k1 v1) (list-rest k2 v2))
                                         (string<=? (symbol->string k1)
                                                    (symbol->string k2))))))
         (sig (md5-string (fold-right (match-lambda* ((list (list-rest k v) acc)
                                                      (string-append (format "~A=~A" k v)
                                                                     acc)))
                                      (setting *FB_SECRET_KEY*)
                                      sans-sig)))
         (json-result (get-url (url+query (setting *FB_API_URL*)
                                          (alist-cons 'sig sig sans-sig))
                               read-json)))
    (if (and (hash? json-result) val-for-key)
        (hash-ref json-result val-for-key #f)
        json-result)))

;;
;; define-facebook-required-login-page
;;
;; Note that all page keyword args are potentially valid except for #:body-wrap, since we
;; use that to get the "required login" functionality.  Also, we force a #:blank #t.
;; 
;; on-login-url (if given) should be a URL relative to your callback URL.
;; E.g., if your callback URL is http://myownserver.com/myfbapp/, then the default is to
;; redirect to the top-level (i.e., passing "").  If you wanted to go to
;; http://myownserver.com/myfbapp/foo/bar
;; you should use "foo/bar".  (Note you shouldn't use a leading slash for on-login-url.)
;;
;; Note in the settings for your app on facebook.com, make sure your callback URL ends in
;; a slash!
;;
(define-syntax define-facebook-required-login-page
  (syntax-rules (=>)
    ((_ (page-name req args ...) => on-login-url
        keywords-and-body ...)
     (define-page (page-name req args ...)
       #:blank #t
       #:body-wrap (lambda (body) (facebook-require-login on-login-url body))
       keywords-and-body ...))
    ((_ (page-name req args ...)
        keywords-and-body ...)
     (define-facebook-required-login-page (page-name req args ...) => ""
       keywords-and-body ...))))

(define (facebook-require-login on-login-url . body)
  `(fb:if-is-app-user
    ,@body
    (fb:else
     (fb:redirect
      ((url ,(format
              "http://www.facebook.com/login.php?v=~A&api_key=~A&next=~A&canvas="
              (setting *FB_API_VERSION*)
              (setting *FB_API_KEY*)
              on-login-url)))))))

;;
;; facebook-session-key
;;
(provide/contract (facebook-session-key (-> request? (or/c #f string?))))
;; 
(define (facebook-session-key req)
  (let ((binds (request-bindings req)))
    (or (assoc-val 'fb_sig_session_key binds)
        (aand (assoc-val 'auth_token binds)
              (facebook-fn "auth.getSession" `((auth_token . ,it))
                           #:val-for-key 'session_key)))))

;;
;; facebook-error
;;
;; returns #f (if the given json result isn't an error) and o/w returns the error msg.
;;
(provide/contract (facebook-error (-> any/c (or/c #f string?))))
;;
(define (facebook-error json-result)
  (and (hash? json-result)
       (hash-ref json-result 'error_msg #f)))

;;
;; facebook-uid
;;
(provide/contract (facebook-uid (-> request? (or/c #f string?))))
;;
(define (facebook-uid req)
  (assoc-val 'fb_sig_user (request-bindings req)))

;;
;; facebook-form
;;
;; Same interface to the standard form function.  A few keyword args are set appropriately
;; for Facebook, though.
;;
(define facebook-form
  (make-keyword-procedure
   (lambda (kws kw-vals . reg-args)
     (call-with-keyword-override form
                                 kws kw-vals
                                 (list '#:action) (list "")
                                 reg-args))))

;;
;; facebook-complex-val
;;
;; The Facebook API sometimes takes parameters of type "complex".  This is just a JSON
;; object.  This function takes standard Scheme bindings and turns them into an appropriate
;; "complex" JSON object.
;;
(provide/contract (facebook-complex-val (-> (listof (cons/c symbol? any/c)) string?)))
;;
(define (facebook-complex-val bindings)
  (js-hash (hash-hash-map (alist->hash bindings) (lambda (k v) (js-quote v)))))

;;
;; facebook-create-object
;;
;; Each key in bindings must have already been created as a property of the given
;; type.
;; Returns the newly created object id (as determined by Facebook).
;;
(provide/contract (facebook-create-object (->* (symbol?
                                                (listof (cons/c symbol? string?))
                                                request?)
                                               (#:association
                                                (or/c #f string?)
                                                #:associate-existing-id-to-fresh
                                                (or/c #f string?))
                                               string?)))
;;
(define (facebook-create-object type bindings req
                                #:association (association #f)
                                #:associate-existing-id-to-fresh (from-id #f))
  (let ((obj-id (facebook-fn "data.createObject"
                             `((obj_type . ,(symbol->string type))
                               (properties . ,(facebook-complex-val bindings))))))
    (aif (facebook-error obj-id)
         (e it)
         (let ((obj-id (number->string obj-id)))
           (when association
             (let ((assoc-result (facebook-fn "data.setAssociation"
                                              `((name . ,association)
                                                (obj_id1 . ,from-id)
                                                (obj_id2 . ,obj-id))
                                              #:pass-session-from-req req)))
               (awhen (facebook-error assoc-result)
                 (e it))))
           obj-id))))

(define (facebook-strict-error fb-fn-result)
  (awhen (facebook-error fb-fn-result) (e it)))
