#lang scheme/base

(require scheme/match
         (planet untyped/dispatch:1:=7/dispatch)
         (planet jaymccarthy/with-bindings:1:=2/with-bindings)
         web-server/servlet-env
         web-server/dispatchers/dispatch
         web-server/configuration/responders
         "util.scm"
         "settings.scm"
         "web-export.ss"
         "web-support.scm"
         "record.scm"
         "repository.scm"
         "form.scm"
         "validate.scm"
         "closures.scm"
         "session.scm"
         "js.scm"
         "user.scm"
         "time.scm"
         "page.scm"
         "compute.scm"
         "profiler.scm"
         "log.scm"
         "task-queue.scm"
         "feed.ss"
         "facebook.ss"
         )

(provide

 ;; the work of others:
 (all-from-out (planet untyped/dispatch:1:=7/dispatch))
 (all-from-out (planet jaymccarthy/with-bindings:1:=2/with-bindings))

 ;; built-in PLT tools:
 request-bindings
 
 ;; web server
 serve
 define-app
 load-server-settings
 server-log
 
 ;; core web help
 web-link
 wrap-each-in-list
 raw-str
 img
 xexpr-if
 url+query
 url->string
 get-url
 
 ;; web forms
 form
 validate
 field-validate
 form-id
 form-markup
 grab-user-input

 ;; feeds 
 atom-feed
 atom-item
 rss-feed
 rss-item
 
 ;; records and the data repository
 rec-prop
 rec-has-prop?
 rec-child-prop
 rec-id
 rec-data
 rec-set-prop!
 rec-set-each-prop!
 rec-remove-prop!
 rec-set-data!
 rec-set-rec-prop!
 rec-rec-prop
 load-rec
 record-id-stored?
 load-where
 load-children
 load-descendants
 contains-child?
 rec-add-child!
 add-child-and-save!
 remove-child-and-save!
 rec-add-list-prop-elt!
 store-rec!
 delete-rec!
 fresh-rec-from-data
 same-rec?
 only-rec-of-type
 if-rec-of-type
 rec-type-is?
 is-descendant?
 find-parent
 find-ancestor
 find-highest-ancestor
 find-incoming-record
 find-incoming-records
 rec?
 sort-recs-by
 define-cache
 define-type-cache
 
 ;; closures
 handle-closure-in-req
 body-as-url
 body-as-closure-key
 num-closures-in-memory
 make-closure-key
 add-closure!
 closure-key->url
 
 ;; sessions
 session-put-val!
 session-get-val
 session-id
 session-get-hash
 session-replace-hash!
 get-session-object
 session-remove-entry!
 sessioned-response
 make-fresh-session
 remove-session
 flash-create!
 flash-get!

 ;; response
 cookied-response
 list-response

 ;; js
 js-script-invoke
 js-array
 js-hash
 js-quote
 js-call
 js-call-on-load
 
 ;; html, pages, includes, etc
 define-page
 define-session-page
 page
 design
 **
 page-url
 redirect-to-page
 (rename-out (response-promise-to-redirect redirect-to))
 js-inc
 css-inc
 versioned-file-reference

 ;; settings
 declare-setting
 setting
 setting-set!

 ;; users
 register-form
 welcome-message
 login-form
 register-form
 register-user!
 make-unloginable-user!
 current-user
 user-in
 created-by?
 created-by-xexpr
 created-by-user-rec
 stamp-user-on-rec!
 get-user-rec
 authenticated-login!
 unauthenticated-login!
 if-these-users
 if-login
 when-login
 logout-user!
 
 ;; time
 created-when
 created-when-str
 days-since
 hours-since
 minutes-since
 A_DAY
 AN_HOUR
 THIRTY_DAYS
 seconds->time-string

 ;; computation
 sum-recs

 ;; profiler
 profile
 define-profile
 
 ;; task queues
 make-threaded-task-queue
 sleep-task-thread-for-at-least
 task-inspector-lock
 task-inspector-num-tasks-thunk

 ;; facebook
 facebook-fn
 define-facebook-required-login-page
 facebook-require-login
 facebook-session-key
 facebook-error
 facebook-uid
 facebook-form
 facebook-complex-val
 facebook-create-object
 facebook-strict-error

 )

(declare-setting *APP_VERSION* 1)
(declare-setting *PAGE_NOT_FOUND_FILE* "page-not-found.html")
(declare-setting *CATCH-EXCEPTION?* (lambda (exn) #t))
(declare-setting *EXCEPTION->XEXPR* (lambda (exn)
                                      (if (exn:dispatcher? exn)
                                          ;; then it really just means that dispatch
                                          ;; failed to find an appropriate URL match, so we
                                          ;; need to look for static files:
                                          (next-dispatcher)
                                          ;; otherwise, it is an actual error...
                                          (begin
                                            ((error-display-handler)
                                             (exn-message exn) exn)
                                            "Error on page."))))

(define (serve web-app)
  (populate-caches)
  (server-log "Server is ready at ~A (ctrl-c to stop it)." (setting *WEB_APP_URL*))
  (serve/servlet #:command-line? #t
                 #:launch-browser? #f
                 #:servlet-path "/"
                 #:server-root-path (build-path ".")
                 #:servlet-regexp #rx""
                 #:file-not-found-responder
                 (gen-file-not-found-responder (build-path
                                                "htdocs"
                                                (setting *PAGE_NOT_FOUND_FILE*)))
                 #:listen-ip (setting *LISTEN_IP*)
                 #:port (setting *PORT*)
                 (lambda (req)
                   (let ((catch? (setting *CATCH-EXCEPTION?*))
                         (err (setting *EXCEPTION->XEXPR*)))
                     (with-handlers ((catch? err))
                       (final-prep-of-response
                        (handle-closure-in-req req
                                               (dispatch req web-app))))))))
                 
(define-syntax define-app
  (syntax-rules ()
    ((_ app-name
        (page-name route-syntax)
        ...)
     (begin (provide app-name page-name ...)
            (define-site app-name ((route-syntax page-name) ...))))))

;; load appropriate settings file based on command line arg to server script
(define (load-server-settings #:envo (envo #f))
  (load (string-append "settings-"
                       (or envo
                           (let ((args (current-command-line-arguments)))
                             (if (= (vector-length args) 0)
                                 "localhost"
                                 (vector-ref args 0))))
                       ".scm")))
