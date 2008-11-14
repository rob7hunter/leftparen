#lang scheme/base

(require "util.scm"
         "form.scm"
         "repository.scm"
         "record.scm"
         "web-support.scm"
         "closures.scm"
         "settings.scm"
         "session.scm"
         "page.scm")

(provide current-user
         user-in
         logout-user!
         register-user!
         make-unloginable-user!
         if-login
         when-login
         get-user-rec
         authenticated-login!
         unauthenticated-login!
         login-form
         register-form
         welcome-message
         created-by?
         created-by-xexpr
         created-by-user-rec
         stamp-user-on-rec!
         if-these-users
         )

;; e.g. (if-these-users '(vegashacker gerstini) sesh
;;                      "you are okay"
;;                      "this is not for you.")
(define-syntax if-these-users
  (syntax-rules ()
    ((_ usernames sesh
        then
        else)
     (let ((u (current-user sesh)))
       (if (aand u (rec-prop u 'username) (member it (map ->string usernames)))
           then
           else)))))

;; binds user-rec-iden to the current user in the body; fails if there is no user.
(define-syntax user-in
  (syntax-rules ()
    ((_  sesh (user-rec-iden) body ...)
     (let ((user-rec-iden (current-user sesh)))
       (if user-rec-iden
           (begin body ...)
           "Authentication error.")))))

(define (stamp-user-on-rec! rec user)
  (rec-set-rec-prop! rec 'created-by user))

(define (get-user-rec username)
  (load-where `((username . ,username))
              #:type 'user
              #:exactly-one #t
              #:equal-fn (lambda (un1 un2) (string=? (string-downcase un1)
                                                     (string-downcase un2)))))

;; returns #f if there is no logged in user
(define (current-user sesh)
  (let ((uid (session-get-val sesh 'logged_in_as)))
    (if (or (not uid) (not (record-id-stored? uid)))
        ;; then there's no user logged in; let's cleanup in case there is an
        ;; entry pointing to a non-existant  user:
        (begin (session-remove-entry! sesh 'logged_in_as)
               #f)
        (load-rec uid))))

(define (logout-user! sesh)
  (session-remove-entry! sesh 'logged_in_as))

;; be careful...no password required
(define (unauthenticated-login! user-rec sesh)
  (session-put-val! sesh 'logged_in_as (rec-id user-rec)))

;; returns a user-rec if successful; #f o/w
(define (authenticated-login! username password sesh)
  (aand (get-user-rec username)
        (and (string=? (md5-string (string-append password (rec-prop it 'salt)))
                       (rec-prop it 'hashed-pass))
             (begin (session-put-val! sesh 'logged_in_as (rec-id it))
                    it))))

(define-syntax if-login
  (syntax-rules ()
    ((_ sesh (user-iden) then else)
     (let ((user-iden (current-user sesh)))
       (if user-iden
           then
           else)))))

(define-syntax when-login
  (syntax-rules ()
    ((_ sesh (user-iden) then ...)
     (let ((user-iden (current-user sesh)))
       (when user-iden then ...)))))

;; MMM any way to redirect back to the "current page" before the closure invocation?

(define (welcome-message sesh
                         #:on-success (success-fn #f)
                         #:no-register (no-register #f))
  (if-login sesh (u)
            (** (format "Welcome, ~A " (rec-prop u 'username))
                (web-link "Sign out" (body-as-url (r)
                                                  (logout-user! sesh)
                                                  (redirect-to (setting *WEB_APP_URL*)))))
            (** (web-link "Sign in" (body-as-url (r) (login-form sesh
                                                                 #:on-success success-fn)))
                (xexpr-if (not no-register)
                          (** " or "
                              (web-link "Register"
                                        (body-as-url
                                         (r)
                                         (register-form sesh
                                                        #:on-success success-fn))))))))

(define (login-form sesh
                    #:on-success (success-fn #f)
                    #:error-wrapper (error-wrapper default-error-wrapper))
  (form '((username "Username" text) (password "Password" password))
        #:skip-save #t
        #:error-wrapper error-wrapper
        #:submit-label "Sign in"
        #:validate (cut user-login-validator <> sesh)
        #:on-done (lambda (bogus-login-rec)
                    (after-login/register-action (current-user sesh) success-fn))))

(define (user-login-validator login-rec sesh)
  (let* ((username (rec-prop login-rec 'username))
         (password (rec-prop login-rec 'password))
         (existing (get-user-rec username)))
    (aif (and username password (authenticated-login! username password sesh))
         #f
         (cond ((not username) "Please enter a username.")
               ((not password) "Please enter a password.")
               (else "Username and password do not match.")))))

(define (register-form sesh
                       #:on-success (success-fn #f)
                       #:error-wrapper (error-wrapper default-error-wrapper))
  (form '((username "Username" text) (password "Password" password)
          (retype-password "Re-type password" password))
        #:error-wrapper error-wrapper
        #:skip-save #t
        #:validate user-registration-validator
        #:submit-label "Sign up!"
        #:on-done (lambda (reg-rec)
                    (after-login/register-action (make-fresh-user reg-rec sesh)
                                                 success-fn))))

(define (after-login/register-action rec success-fn)
  (if success-fn
      (success-fn rec)
      (redirect-to (setting *WEB_APP_URL*))))

(define (user-registration-validator reg-data-rec)
  (let* ((pw (rec-prop reg-data-rec 'password))
         (pw-re (rec-prop reg-data-rec 'retype-password))
         (username (rec-prop reg-data-rec 'username))
         (existing (get-user-rec username)))
    (cond ((not username) "Please enter a username.")
          ((not (pregexp-match "^[a-zA-Z0-9]+$" username))
           "Usernames can only contain letters and numbers.")
          ((> (string-length username) 15)
           "Usernames can be at most 15 characters.")
          ((< (string-length username) 2)
           "Usernames must be at least 2 characters.")
          (existing (format "The username '~A' is already taken."
                            username))
          ((not pw) "Please enter a password.")
          ((and pw pw-re (string=? pw pw-re)) #f)
          (else "Passwords don't match."))))

;; returns #f if everything was fine, or a str error message o/w
(define (register-user! username pass sesh)
  (let ((throw-away (fresh-rec-from-data `((username . ,username)
                                           (password . ,pass)
                                           (retype-password . ,pass)))))
    (or (user-registration-validator throw-away)
        (let* ((salt (random-key-string 20))
               (hashed-pass (md5-string (string-append pass salt)))
               (new-user (fresh-rec-from-data `((type . user)
                                                (username . ,username)
                                                (hashed-pass . ,hashed-pass)
                                                (salt . ,salt))
                                              #:stamp-time #t)))
          (store-rec! new-user)
          (unauthenticated-login! new-user sesh)
          #f))))

;;
;; make-unloginable-user!
;;
;; the only way to log in such a user is to use unauthenticated-login!.  Why would
;; you want this?  Maybe you want your app to have a notion of users that is
;; transparent to your actual user. E.g., if you have a unique key for each user
;; (like an iPhone device ID), then you can use that as their username, and then
;; the user wouldn't need a password, and they wouldn't need to login.
;; returns #f if the username is already taken; o/w returns the fresh user rec.
;; The freshly created user is logged in.
;;
(define (make-unloginable-user! username sesh)
  (and-let* (((not (get-user-rec username)))
             (fresh-user (fresh-rec-from-data `((type . user)
                                                (username . ,username))
                                              #:stamp-time #t)))
    (store-rec! fresh-user)
    (unauthenticated-login! fresh-user sesh)
    fresh-user))

;; XXX need to do validation here...build some in to the forms engine
(define (make-fresh-user user-reg-rec sesh)
  (let* ((pass (rec-prop user-reg-rec 'password))
         (salt (random-key-string 20))
         (hashed-pass (md5-string (string-append pass salt)))
         (new-user (fresh-rec-from-data `((type . user)
                                          (username . ,(rec-prop user-reg-rec 'username))
                                          (hashed-pass . ,hashed-pass)
                                          (salt . ,salt))
                                        #:stamp-time #t)))
    (store-rec! new-user)
    (unauthenticated-login! new-user sesh)
    new-user))

(define (created-by? rec user-rec)
  (aand (rec-prop rec 'created-by)
        (string=? it (rec-id user-rec))))

;; returns "" if no creator is available in the given rec.
;; if #:link is given, then make the name a link to the given link
(define (created-by-xexpr rec #:link (link #f) #:by-text (by-text "by "))
  (aif (aand (rec-rec-prop rec 'created-by) (rec-prop it 'username))
       `(group ,by-text 
               ,(if link (web-link it link) it))
       ""))

(define (created-by-user-rec rec)
  (rec-rec-prop rec 'created-by))
