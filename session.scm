#lang scheme/base

(require (planet "web.scm" ("soegaard" "web.plt" 2 1))
         "web-support.scm"
         (file "util.scm")
         (file "time.scm")
         (file "record.scm")
         (file "repository.scm")
         (lib "time.ss" "srfi" "19"))

(provide session-put-val!
         session-get-val
         session-id
         session-get-hash
         session-replace-hash!
         get-session-object
         session-remove-entry!
         sessioned-response
         make-fresh-session
         remove-session
         cookied-response
         cookie-val
         flash-create!
         flash-get!
         )

;; a session is a map from keys to values
;; we store a key to a session in a client cookie with key "sesh" and the session id
;; as the value.

(define SESSION_KEY_LENGTH 20) ; totally made up

(define (make-fresh-session)
  (let ((rec (fresh-rec-from-data '((type . session))
                                  #:stamp-time #t
                                  #:id-length SESSION_KEY_LENGTH)))
    (store-rec! rec)
    (values (rec-id rec) rec)))

(define (remove-session sesh)
  (delete-rec! sesh))
  
(define (get-session-object session-key)
  (and (record-id-stored? session-key)
       (load-rec session-key)))

(define (session-put-val! sesh key val)
  (rec-set-prop! sesh key val)
  (store-rec! sesh)
  sesh)

(define (session-get-hash sesh)
  (rec-data sesh))

(define (session-replace-hash! sesh hash)
  (rec-set-data! sesh hash)
  (store-rec! sesh)
  sesh)

(define (session-get-val sesh key (missing-val #f))
  (rec-prop sesh key missing-val))

(define (session-id sesh)
  (rec-id sesh))

(define (session-remove-entry! sesh key)
  (rec-remove-prop! sesh key)
  (store-rec! sesh)
  sesh)

;; evaluates the body, binding sesh-iden to a sesh (either existing or fresh).
;; the latter case happens if this is their first time hitting the page from that browser,
;; if they logged out, or if they have cookies off.
(define-syntax sessioned-response
  (syntax-rules ()
    ((_ req (sesh-iden) body ...)
     (let* ((sesh-id (cookie-val req "sesh"))
            (sesh-iden (and sesh-id (get-session-object sesh-id))))
       (if (not sesh-iden)
           (receive (fresh-sesh-id sesh-iden) (make-fresh-session)
             (cookied-response "sesh" fresh-sesh-id
                               body ...))
           (let ((body-lst (list body ...)))
             (or (single-response-promise-in-list body-lst)
                 `(group ,@body-lst))))))))

(define (cookied-response cookie-key-str cookie-val-str
                          #:expire-in (expire-in THIRTY_DAYS)
                          . content-lst)
  (let ((headers (list (make-header #"Set-Cookie"
                                    (string->bytes/utf-8
                                     (format "~A=~A; expires=~A; path=~A"
                                             cookie-key-str cookie-val-str
                                             (cookie-expiry-time expire-in)
                                             "/"))))))
    (aif (single-response-promise-in-list content-lst)
         (response-from-promise it #:headers headers)
         (list-response content-lst #:extras headers))))

(define (cookie-expiry-time secs-from-now)
  (date->string (time-utc->date (make-time 'time-utc 0
                                           (+ (current-seconds) secs-from-now))
                                0)
                "~a, ~d-~b-~Y ~T GMT"))

;; returns #f if no matching key-str is found; o/w returns a str val
(define (cookie-val req key-str)
  (and-let* ((header-binds (request-headers req))
             ((exists-binding? 'cookie header-binds))
             (cookie-strs (pregexp-split "; "
                                         (extract-binding/single 'cookie
                                                                 (request-headers req)))))
    (any (lambda (kv-str)
           (match (pregexp-split "=" kv-str)
                  ((list key val)
                   (and (string=? key-str key) val))
                  (else #f)))
         cookie-strs)))

;;
;; flash implementation
;;
;; A flash is a some data that stays until it is used.  The classic example is with
;; messages that persist to the user just one time, like "Your message was sent."
;;

(define (flash-create! sesh key val)
  (session-put-val! sesh key val))

(define (flash-get! sesh key)
  (let ((val (session-get-val sesh key)))
    (session-remove-entry! sesh key)
    val))
