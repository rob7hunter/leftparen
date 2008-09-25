#lang scheme/base

;; support for global settings

(require (file "util.scm"))

(provide declare-setting
         setting
         setting-set!)
         
(define SETTING_STORAGE (hash))
(define MISSING_VALUE '*MISSING_VALUE*)

(define-syntax declare-setting
  (syntax-rules ()
    ((_ setting-name)
     (setting-set-aux! 'setting-name MISSING_VALUE #t))
    ((_ setting-name default-val)
     (setting-set-aux! 'setting-name default-val #t))))

(define-syntax setting
  (syntax-rules ()
    ((_ setting-name)
     (let* ((sname 'setting-name)
            (result (hash-ref SETTING_STORAGE sname
                              (lambda () (no-setting-error sname)))))
       (if (eq? result MISSING_VALUE)
           (no-setting-value-error sname)
           result)))))

;; you can't use setting-set! on a setting unless it has previously been declared with
;; declare-setting.
(define-syntax setting-set!
  (syntax-rules ()
    ((_ setting-name new-val)
     (setting-set-aux! 'setting-name new-val #f))))

(define (setting-set-aux! key val is-default)
  (let ((does-exist (hash-exists? SETTING_STORAGE key)))
    (if (or (and is-default does-exist)
            (and (not is-default) (not does-exist)))
        (no-setting-error key)
        (hash-set! SETTING_STORAGE key val))))

(define (no-setting-error setting-name)
  (e "No setting named '~A' exists." setting-name))

(define (no-setting-value-error setting-name)
  (e "No value was set for setting named '~A'." setting-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; declare core settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare-setting *WEB_APP_URL*)
(declare-setting *PORT*)
(declare-setting *LISTEN_IP*)
(declare-setting *CLOSURE_URL_KEY* 'function)
