#lang scheme/base

(require (file "../leftparen/util.scm"))

(provide profile
         define-profile)

;; a map from keys to num of times that block was called:
(define *PROFILE_NUM_CALLS* (make-hash))

;; a map from keys to total amount of time spent in that block:
(define *PROFILE_TOTAL_TIME* (make-hash))

(define-struct profile-frame (key entry-time subtract-time) #:mutable)

;; a list of profile frames
(define *PROFILE_CALL_STACK* '())

;;
;; define-profile
;;
;; Just change a (define (f arg ...) body ...) into a (define-profile (f arg ...) body ...)
;; and you're off to the races.
;;
(define-syntax define-profile
  (syntax-rules ()
    ((_ (fn arg ...) body ...)
     (define (fn arg ...)
       (profile :: fn begin body ...)))))

;;
;; profile
;;
;; Usage: if you have a call like (f arg ...), you can profile it by changing it
;; to (profile f arg ...).  The result will appear in the profile results as "f".
;; If "f" is an expression itself, or for some other reason wouldn't make a good
;; name in the profile results, then you can use (profile :: 'another-key f arg ...).
;; Note that the key is not auto-quoted.
;;
(define-syntax profile
  (syntax-rules (::)
    ((_ :: profile-key-name f arg ...)
     (let ((profile-key 'profile-key-name))
       (when (empty? *PROFILE_CALL_STACK*)
         (display (format "---Starting profile for '~A'---\n" profile-key)))
       (inc-call-count-for! profile-key)
       (start-timer-for! profile-key)
       (let ((result (f arg ...)))
         (stop-timer-for! profile-key)
         (when (empty? *PROFILE_CALL_STACK*)
           (display (format "~A\n---End profile for '~A'---\n\n"
                            (profile-data-str) profile-key))
           (clear-profile-data!))
         result)))
    ((_ f arg ...)
     (profile :: f f arg ...))))

(define (clear-profile-data!)
  (set! *PROFILE_NUM_CALLS* (make-hash))
  (set! *PROFILE_TOTAL_TIME* (make-hash)))

(define (inc-call-count-for! profile-key)
  (hash-set! *PROFILE_NUM_CALLS* profile-key
             (+ 1 (hash-ref *PROFILE_NUM_CALLS* profile-key 0))))

(define (start-timer-for! profile-key)
  (set! *PROFILE_CALL_STACK* (cons (make-profile-frame
                                    profile-key (current-milliseconds) 0)
                                   *PROFILE_CALL_STACK*)))

(define (stop-timer-for! profile-key)
  (let* ((cur-frame (first *PROFILE_CALL_STACK*))
         (cur-profile-key (profile-frame-key cur-frame))
         (cur-entry-time (profile-frame-entry-time cur-frame))
         (cur-sub-time (profile-frame-subtract-time cur-frame))
         (cur-total-time (- (current-milliseconds) cur-entry-time)))
    ;; pop off the current frame:
    (set! *PROFILE_CALL_STACK* (rest *PROFILE_CALL_STACK*))
    ;; just some program checking:
    (unless (eq? cur-profile-key profile-key)
      (e "Logic error with mis-matched profiling keys (~A and ~A)"
         cur-profile-key profile-key))
    (let ((adjusted-time (- cur-total-time cur-sub-time)))
      ;; add in the adjusted-time to that key's total:
      (hash-set! *PROFILE_TOTAL_TIME* profile-key
                 (+ adjusted-time (hash-ref *PROFILE_TOTAL_TIME* profile-key 0)))
      ;; you need to subtract out time from the parent too:
      (unless (empty? *PROFILE_CALL_STACK*)
        (let* ((parent-frame (first *PROFILE_CALL_STACK*))
               (parent-sub-time (profile-frame-subtract-time parent-frame)))
          ;; mutate the parent:
          (set-profile-frame-subtract-time! parent-frame
                                            (+ parent-sub-time cur-total-time)))))))

(define (profile-data-str)
  (string-join (hash-map *PROFILE_NUM_CALLS*
                         (lambda (k v)
                           (format "~A: ~A calls in ~A milliseconds"
                                   k v (hash-ref *PROFILE_TOTAL_TIME* k))))
               "\n"))
