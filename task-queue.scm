#lang scheme/base

(require "util.scm")

(provide make-threaded-task-queue sleep-task-thread-for-at-least)

;; returns a VALUES of a thread that is suspended and a procedure for adding thunk tasks
; to the queue
(define (make-threaded-task-queue)
  (let ((queue (make-task-queue))
        (just-created #t))
    (letrec ((thread-thunk (lambda ()
                             (when just-created
                               (set! just-created #f)
                               (thread-suspend the-thread))
                             (let lp ()
                               (let ((msg (thread-try-receive)))
                                 (if msg
                                     (let ((wait-until (task-queue-msg-wait-until msg))
                                           (now (current-seconds)))
                                       (if (> wait-until now)
                                           ;; then we found a relevant wait until msg
                                           (begin (sleep (- wait-until now))
                                                  (thread-thunk))
                                           ;; otherwise, the msg wasn't valid, so try again
                                           (lp)))
                                     ;; o/w we attack the queue
                                     (let ((more? (task-pop-and-handle! queue)))
                                       (if more?
                                           (thread-thunk)
                                           (begin (thread-suspend the-thread)
                                                  (thread-thunk)))))))))
             (the-thread (thread thread-thunk)))
      (values the-thread (lambda (task-thunk)
                           (task-push! queue task-thunk) 
                           (thread-resume the-thread))))))

(define-struct task-queue-msg (wait-until))

(define (sleep-task-thread-for-at-least a-thread secs-to-wait)
  (thread-send a-thread (make-task-queue-msg (+ (current-seconds) secs-to-wait))))

;; it should be FIFO to preserve semantics as much as possible

;; IDEA: have tasks that are "infinite" ..i.e., once you execute one, it pops back on
;; to the end of the queue.
      
(define-struct task-queue-dat (lock lst last-task-ptr) #:mutable)

(define (make-task-queue)
  (make-task-queue-dat (make-lock) '() #f))

(define (tasks-todo? queue)
  (not (null? (task-queue-dat-lst queue))))

;; executes the next task if one is available; returns #t if there are more tasks left
;; (after the one just executed).   #f o/w.
(define (task-pop-and-handle! queue)
  (sync-on-lock
   (task-queue-dat-lock queue)
   (let ((lst (task-queue-dat-lst queue)))
     (when (tasks-todo? queue)
       (let ((next-task (mcar lst)))
         (set-task-queue-dat-lst! queue (mcdr lst))
         ;; next-task is a thunk...so we invoke it
         (next-task)))
     (tasks-todo? queue))))

(define (task-push! queue task-thunk)
   (let ((cell-for-end (mcons task-thunk '())))
     (sync-on-lock
      (task-queue-dat-lock queue)
      (if (not (tasks-todo? queue))
          (set-task-queue-dat-lst! queue cell-for-end)
          (set-mcdr! (task-queue-dat-last-task-ptr queue) cell-for-end))
      (set-task-queue-dat-last-task-ptr! queue cell-for-end))))
   
