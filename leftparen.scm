#lang scheme/base

(require scheme/match
         (planet "web.scm" ("soegaard" "web.plt" 2 1))
         (planet "instaservlet.ss" ("untyped" "instaservlet.plt" 1 7))
         (planet "dispatch.ss" ("untyped" "dispatch.plt" 1 5))
         "util.scm"
         "settings.scm"
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
         )

(provide

 ;; the work of others:
 (except-out (all-from-out (planet "web.scm" ("soegaard" "web.plt" 2 1)))
             comment?)
 (all-from-out (planet "dispatch.ss" ("untyped" "dispatch.plt" 1 5)))
 (all-from-out (planet "instaservlet.ss" ("untyped" "instaservlet.plt" 1 7)))
 
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

 )

(declare-setting *APP_VERSION* 1)

(declare-setting *CATCH-EXCEPTION?* (lambda (exn) #t))
(declare-setting *EXCEPTION->XEXPR* (lambda (exn)
                                      ((error-display-handler) (exn-message exn) exn)
                                      "Page not found."))

;; consumes a web-app; passes its keyword arguments to go! (part of instaservlet)
(define serve
  (make-keyword-procedure
   (lambda (kws kw-vals . reg-args)
     (match reg-args
            ((list)
             (e "The serve function requires you to pass an app as the first argument."))
            ((list web-app)
             (begin
               (populate-caches)
               (keyword-apply go!
                              kws
                              kw-vals
                              (list (lambda (req)
                                       (let ((catch? (setting *CATCH-EXCEPTION?*))
                                             (err (setting *EXCEPTION->XEXPR*)))
                                         (with-handlers ((catch? err))
                                           (final-prep-of-response
                                            (handle-closure-in-req
                                             req
                                             (dispatch req web-app))))))))))))))

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
