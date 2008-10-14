#lang scheme/base

(require "util.scm")

;;
;; Task Queues
;;
;; This library allows you to create synchronized "task queues".  In normal use,
;; you add tasks (which are just thunks) onto the queue.  Meanwhile, the queue itself
;; tries to evaluate the thunks, in FIFO order, one at a time.  When the thread runs
;; out of tasks, then it suspends itself.  But, when a new task is received, the
;; thread is automatically resumed, so, in effect, it's always ready to work.
;;
;; Use (make-threaded-task-queue) to generate a VALUES of a thread that is suspended
;; (which contains an empty task queue), and a procedure of one argument (a task thunk)
;; which is the task adder.  You may never need to use the returned thread, but if
;; you want to put a task thread on hiatus, use, e.g., 
;;  (sleep-task-thread-for-at-least <task-thread> 30)
;; which suspends processing of tasks for at least 30 seconds.  In practice, the time
;; the thread waits should be just every so slightly more than the given seconds value.
;;
;; We allow for "one off" LIFO queue semantics.  That is, if you add a task using
;; the task adder function (assume you've named it add!), you can write
;;  (add! (lambda () ...some task...) #:add-to-front #t)
;; and this task will be the next one to be chosen for evaluation.
;;

(provide make-threaded-task-queue
         sleep-task-thread-for-at-least
         task-inspector-lock
         task-inspector-num-tasks-thunk)

;; returns a VALUES of a thread that is suspended and a procedure for adding thunk tasks
;; to the queue; if return-inspector is #t, return a third value which is an inspector
;; object, which can be accessed with the task-inspector-* fns.
(define (make-threaded-task-queue #:return-inspector (return-inspector #f))
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
                                           ;; so we received a message that told us to
                                           ;; wait...so sleep the thread for that long:
                                           (begin (sleep (- wait-until now))
                                                  (thread-thunk))
                                           ;; otherwise, the msg is no longer relevant,
                                           ;; so try again:
                                           (lp)))
                                     ;; no msg?  then attack the queue:
                                     (let ((more? (task-pop-and-handle! queue)))
                                       (if more?
                                           (thread-thunk)
                                           (begin (thread-suspend the-thread)
                                                  (thread-thunk)))))))))
             (the-thread (thread thread-thunk)))
      (let ((add-task-fn (lambda (task-thunk #:add-to-front (add-to-front #f))
                           (task-push! queue task-thunk #:add-to-front add-to-front)
                           (thread-resume the-thread))))
        (if return-inspector
            (values the-thread add-task-fn (make-a-task-inspector queue))
            (values the-thread add-task-fn))))))

(define-struct task-inspector (lock num-tasks-thunk))

(define (make-a-task-inspector queue)
  (make-task-inspector (task-queue-dat-lock queue)
                       (lambda () (length (task-queue-dat-lst queue)))))

(define-struct task-queue-msg (wait-until))

(define (sleep-task-thread-for-at-least a-thread secs-to-wait)
  (thread-send a-thread (make-task-queue-msg (+ (current-seconds) secs-to-wait))))

;; IDEA: have tasks that are "infinite" ..i.e., once you execute one, it pops back on
;; to the end of the queue.
      
(define-struct task-queue-dat (lock lst last-task-ptr) #:mutable)

(define (make-task-queue)
  (make-task-queue-dat (make-lock) '() #f))

;; We implement the queue as a mutable list, where the next task to do is the
;; car of the list.  We keep a last task pointer so that we can easily add to the end
;; of the queue.

;; executes the next task if one is available; returns #t if there *are* more tasks left
;; (after the one just executed).   returns #f if no tasks are available.
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

(define (task-push! queue task-thunk #:add-to-front (add-to-front #f))
   (let ((cell-for-end (mcons task-thunk '())))
     (sync-on-lock
      (task-queue-dat-lock queue)
      (if (not (tasks-todo? queue))
          (set-task-queue-dat-lst! queue cell-for-end)
          (if add-to-front
              (set-task-queue-dat-lst! queue (mcons task-thunk (task-queue-dat-lst queue)))
              (set-mcdr! (task-queue-dat-last-task-ptr queue) cell-for-end)))
      (unless add-to-front
        (set-task-queue-dat-last-task-ptr! queue cell-for-end)))))

;; not synchronized, so make sure you use in a sync block.
(define (tasks-todo? queue)
  (not (null? (task-queue-dat-lst queue))))
