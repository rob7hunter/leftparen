#lang scheme/base

(require scheme/list
         (only-in (lib "1.ss" "srfi")
                  reverse! zip unzip1 unzip2 (remove removef)
                  delete-duplicates! concatenate any iota
                  alist-cons break cons* delete-duplicates every fold-right reduce find
                  lset-difference lset-union pair-fold-right span take delete
                  drop fold pair-fold delete!
                  )
         (lib "26.ss" "srfi")
         (lib "2.ss" "srfi")
         (only-in (lib "13.ss" "srfi")
                  string-join string-trim string-trim-right string-trim-both
                  string-reverse string-reverse!)
         (lib "pregexp.ss")
         
         mzlib/defmacro
         (for-syntax scheme/base)
         scheme/match
         (lib "pretty.ss")
         (planet "web.scm" ("soegaard" "web.plt" 2 1)) ; XXX get rid of this dependency
         (lib "unit.ss")

         (only-in  file/md5 md5)
         
         )

(provide first
         second
         rest
         empty?
         sort
         vector-for-each
         vector-list-map
         map-i
         for-each-i
         replace-i
         transform-i
         iota
         zip
         unzip1
         unzip2
         concatenate
         take
         take-up-to
         drop
         drop-up-to
         partition
         span
         break
         safe-list-ref
         last
         last-pair
         length=
         length>
         assoc-val
         alist-key-filter
         repeat-thunk-in-list
         cut
         cute
         cross
         filter
         filter-map
         append-map
         removef
         delete
         delete!
         delete-duplicates
         delete-duplicates!
         find
         any
         every
         hash
         map-hash
         sub-hash-set!
         hash-exists?
         hash-keys
         hash-singleton-value
         hash-filter-map
         hash-hash-map
         hash-find
         bucketed-hash-add!
         fold ;(iterative-style)
         fold-right ;(recursive-style)
         reduce
         reduce-right-result
         pair-fold
         pair-fold-right
         file-line-fold
         cons*
         cons-to-end
         listify
         alist-cons
         alist-merge
         receive
         
         aif
         awhen
         aand
         and-let*
         
         pregexp-split
         pregexp-match
         pregexp-match-positions
         pregexp-replace
         pregexp-replace*
         pregexp-replace-many
         regexp-replace-in-list*
         
         string-join
         string-ellide
         capitalize-word
         string-trim
         string-trim-right
         string-trim-both
         string-reverse
         string-reverse!
         ->string
         
         pretty-print
         pretty-string
         
         lset-difference
         lset-union
         
         random-choice
         random-choice-and-remove
         random-sub-list
         random-key-string
         
         e
         round-k
         
         show
         prn
         
         match-lambda
         match
         
         xexpr->string ; XXX implement yourself!
         
         splice-if
         asplice-if

         call-with-keyword-override
         make-recursive-keyword-version-of-fn

         max-f
         max-f-elt

         sync-on-lock
         make-lock

         md5-string
         )

(define (random-choice lst)
  (list-ref lst (random (length lst))))

(define (random-sub-list lst)
  (cond ((empty? lst) '())
        ((= (random 2) 0) (cons (first lst) (random-sub-list (rest lst))))
        (else (random-sub-list (rest lst)))))

(define (repeat-thunk-in-list thunk n)
  (let ((result '()))
    (let lp ((n n))
      (if (zero? n) result (begin (set! result (cons (thunk) result)) (lp (- n 1)))))))

(define random-key-string
  (let* ((choices '("b" "c" "d" "f" "g" "h" "j" "k" "m" "n" "p" "q" "r" "s" "t" "u" "v"
                    "x" "y" "z" "2" "3" "4" "5" "6" "7" "8" "9"))
         (len (length choices)))
    (lambda (key-len) (apply string-append (repeat-thunk-in-list
                                            (lambda () (list-ref choices (random len)))
                                            key-len)))))

(define (length= lst n)
  (= (length lst) n))

(define (length> lst n)
  (> (length lst) n))

(define-syntax show
  (syntax-rules ()
    ((_ expr)
     (let ((val expr))
       (display (format "Expr ~A => ~A\n" 'expr val))
       val))))

;; like show, but return "" instead of the value computed; also takes any number of
;; expressions; useful in web debugging because the return value ("") won't affect the
;; page if used.
(define-syntax prn
  (syntax-rules ()
    ((_ expr ...)
     (begin (show expr) ... ""))))

;; returns VAL X LST
(define (random-choice-and-remove lst)
  (let ((to-go (random (length lst)))
        (result '()))
    (let lp ((i 0) (lst lst))
      (if (= i to-go)
          (values (first lst) (append (reverse! result) (rest lst)))
          (begin (set! result (cons (first lst) result))
                 (lp (+ i 1) (rest lst)))))))

(define-syntax receive
  (syntax-rules ()
    ((_ (var ...) values-expr body ...)
     (let-values (((var ...) values-expr)) body ...))))

(define (map-i f . lsts)
  (let lp ((i 0) (lst-ptrs lsts))
    (if (null? (first lst-ptrs))
        '()
        (cons (apply f i (map first lst-ptrs))
              (lp (+ i 1) (map rest lst-ptrs))))))

(define (replace-i lst i new-elt)
  (transform-i lst i (lambda (x) new-elt)))

(define (transform-i lst i f)
  (map-i (lambda (j elt) (if (= j i) (f elt) elt)) lst))

(define-syntax hash
  (syntax-rules (=)
    ((_ (key = val) ...)
     (let ((ht (make-hash)))
       (hash-set! ht `key val) ...
       ht))))

(define (vector-for-each fn . vs)
  (let ((len (vector-length (first vs))))
    (let lp ((i 0))
      (if (>= i len)
          'done
          (begin (apply fn (map (lambda (v) (vector-ref v i)) vs))
                 (lp (+ 1 i)))))))

(define (vector-for-each-i fn . vs)
  (let ((len (vector-length (first vs))))
    (let lp ((i 0))
      (if (>= i len)
          'done
          (begin (apply fn i (map (lambda (v) (vector-ref v i)) vs))
                 (lp (+ 1 i)))))))

(define (vector-list-map fn . vs)
  (let ((len (vector-length (first vs))))
    (let lp ((i 0))
      (if (>= i len)
          '()
          (cons (apply fn (map (lambda (v) (vector-ref v i)) vs))
                (lp (+ 1 i)))))))

;; mutates starting-vector
(define (make-counter! starting-vector ending-vector)
  (let ((len (vector-length starting-vector)))
    ;; returns #f when done
    (lambda ()
      (let lp ((i (- len 1)))
        (and (>= i 0)
             (let ((cur (+ 1 (vector-ref starting-vector i))))
               (vector-set! starting-vector i cur)
               (if (<= cur (vector-ref ending-vector i))
                   starting-vector
                   (begin (vector-set! starting-vector i 0)
                          (lp (- i 1))))))))))

(define (for-each-i fn . lists)
  (let lp ((i 0) (lists lists))
    (if (null? (first lists))
        'done
        (begin (apply fn i (map first lists))
               (lp (+ i 1) (map rest lists))))))

;; can't this be shorter?
(define (cross . lsts)
  (if (= (length lsts) 1)
      (zip (first lsts))
      (let ((rst (apply cross (rest lsts))))
        (append-map (lambda (next)
                      (map (lambda (cons-result)
                             (cons next cons-result))
                           rst))
                    (first lsts)))))

;; fn : elt -> (VALUES k v)
(define (map-hash fn lst)
  (let ((ht (make-hash)))
    (for-each (lambda (elt) (receive (k v) (fn elt) (hash-set! ht k v)))
              lst)
    ht))

(define (hash-exists? ht k)
  (let* ((does-exist #t)
         (failure-thunk (lambda () (set! does-exist #f))))
    (hash-ref ht k failure-thunk)
    does-exist))

(define (hash-keys ht)
  (hash-map ht (lambda (k v) k)))

(define (hash-singleton-value ht)
  (if (= (hash-count ht) 1)
      (hash-iterate-value ht (hash-iterate-first ht))
      (error (format "Exactly one value expected in hash table ~A." ht))))

;; for creating hash-tables within hash-tables, when the outer-key might not exist
;; (in this case, we create a fresh sub-hash-table)
(define (sub-hash-set! outer-ht outer-key inner-key val)
  (let ((has-outer-key (hash-exists? outer-ht outer-key)))
    (unless has-outer-key
      (hash-set! outer-ht outer-key (make-hash)))
    (let ((inner-ht (hash-ref outer-ht outer-key)))
      (hash-set! inner-ht inner-key val))))

(define (hash-filter-map ht fn)
  (removef not (hash-map ht fn)))

;; returns a new hash-table:
(define (hash-hash-map ht fn)
  (let ((fresh-ht (make-hash)))
    (hash-for-each ht (lambda (k v) (hash-set! fresh-ht k (fn k v))))
    fresh-ht))

;; fn : key X val -> #f | alpha
(define (hash-find ht fn)
  (aand (find (lambda (k) (fn k (hash-ref ht k))) (hash-keys ht))
        (hash-ref ht it)))

(define (bucketed-hash-add! bht key val)
  (hash-set! bht key (cons val (hash-ref bht key '()))))

;; f : line-str X acc -> acc'
(define (file-line-fold f initial file-name)
  (with-input-from-file file-name
    (lambda ()
      (let lp ((putative-line (read-line)) (acc initial))
        (if (eof-object? putative-line)
            acc
            (lp (read-line) (f putative-line acc)))))))

(define-macro (aif a b c)
  `(let ((it ,a))
     (if it ,b ,c)))

(define-macro (awhen test . body)
  `(let ((it ,test))
     (if it (begin ,@body) 'done)))

(define-macro (aand . args)
  (if (null? args)
      #t
      (if (null? (cdr args))
          (car args)
          `(let ((it ,(car args)))
             (if it (aand ,@(cdr args)) #f)))))

(define (pretty-string v)
  (let ((p (open-output-string)))
    (pretty-print v p)
    (get-output-string p)))

;;
;; pregexp-replace-many
;;
;; E.g.,
;; (pregexp-replace-many some-str
;;                       ("\n" => " ")
;;                       ("foo" => "bar"))
;;
(define-syntax pregexp-replace-many
  (syntax-rules (=>)
    ((_ str (pattern => replacement) ...)
     (let ((result str))
       (set! result (pregexp-replace* pattern result replacement))
       ...
       result))))

;;
;; regexp-replace-in-list*
;;
;; like pregexp-replace* but the return result is a list of strings and alphas,
;; where alpha is the return type of your function match->xexpr.
;; Unlike pregexp-replace*, the third argument must be a function, and it must
;; take one argument (thus the regexp must only match one thing).
;;
;; You can optionally provide a 4th arg function which will be applied to segments
;; of the given str that don't match.  This can be useful when you have more than
;; one potential transform to apply to a string.
;;
;; Example: (regexp-replace-in-list* "ab" "abbracadabra" (lambda (match) "!"))
;; ==> ("!" "bracad" "!" "ra")
;;
;; Note: this is regexp, not pregexp (Perl regexp), so it's missing some features.
;;  Pregexp "uses { and } bounded repetition and uses \ for meta-characters both
;;  inside and outside of ranges."
;;
(define (regexp-replace-in-list* regexp str match->xexpr
                                  (non-match->xexpr (lambda (x) x)))
  (let lp ((matches (regexp-match-positions* regexp str))
           (idx 0))
    (if (empty? matches)
        (let ((len (string-length str)))
          (if (= idx len)
              (list)
              (list (non-match->xexpr (substring str idx (string-length str))))))
        (let* ((from-idx (caar matches))
               (to-idx (cdar matches))
               (left-str (substring str idx from-idx))
               (matched-str (substring str from-idx to-idx))
               (result (match->xexpr matched-str)))
          (append (if (string=? "" left-str)
                      (list result)
                      (list (non-match->xexpr left-str) (match->xexpr matched-str)))
                  (lp (rest matches) to-idx))))))

(define (assoc-val key alist (missing-val #f))
  (let ((lookup (assoc key alist)))
    (if lookup (cdr lookup) missing-val)))

;; returns an alist where keys are eq are "merged".  alists further to the right
;; overshadow those to the left. duplicate keys are removed.
(define (alist-merge . alists)
  (delete-duplicates! (concatenate (reverse alists))
                      (lambda (pair1 pair2) (eq? (car pair1) (car pair2)))))

(define (alist-key-filter fn alist)
  (filter (match-lambda ((list-rest k v) (fn k))) alist))

(define (cons-to-end elt lst)
  (append lst (list elt)))

(define (e format-str . args)
  (error (apply format format-str args)))

(define (take-up-to lst n)
  (if (or (zero? n) (empty? lst))
      '()
      (cons (first lst) (take-up-to (rest lst) (- n 1)))))

(define (drop-up-to lst n)
  (if (or (zero? n) (empty? lst))
      lst
      (drop-up-to (rest lst) (- n 1))))

;; usage: inside a backquote, ,@(splice-if TEST VAL) or ,@(splice-if TEST-AND-VAL)
(define-syntax splice-if
  (syntax-rules ()
    ((_ test val)
     (if test (list val) '()))
    ((_ test)
     (let ((t test))
       (if t (list t) '())))))

(define-macro (asplice-if test val)
  `(let ((it ,test))
     (splice-if it val)))

;; returns a string of at most n chars.  Uses ellipsis if it has to chop.
(define (string-ellide str n)
  (let ((len (string-length str)))
    (if (<= len (- n 3))
        str
        (string-append (substring str 0 (- n 4)) "..."))))

;; if idx < 0 then returns 0th elt
;; if idx > len-1, then returns (len-1)th elt
;; it's still an error to use on an empty list.
(define (safe-list-ref lst idx)
  (if (< idx 0)
      (first lst)
      (let ((len (length lst)))
        (if (>= idx len)
            (list-ref lst (- len 1))
            (list-ref lst idx)))))

;; XXX should use srfi 13 version of string-upcase, but can't figure out how to get the
;; conflicts working with the mzscheme version.
(define (capitalize-word str)
  (let ((chars (string->list str)))
    (list->string (cons (char-upcase (first chars)) (rest chars)))))

(define (make-recursive-keyword-version-of-fn fn recur-kw-str)
  (make-keyword-procedure
   (lambda (kws kw-vals . reg-args)
     (define recur
       (make-keyword-procedure
        (lambda (override-kws override-kw-vals . override-reg-args)
          (call-with-keyword-override fn
                                      kws kw-vals
                                      (cons (string->keyword recur-kw-str)
                                            override-kws)
                                      (cons recur override-kw-vals)
                                      (if (empty? override-reg-args)
                                          reg-args
                                          override-reg-args)))))
     (recur))))

;; call fn with original kws/kw-vals except override with new kws/kw-vals:
(define (call-with-keyword-override fn
                                    original-kws original-kw-vals
                                    new-kws new-kw-vals
                                    reg-args)
  (receive (kws kw-vals)
      (unzip2 (sort (lset-union (lambda (k1.v1 k2.v2) (eq? (car k1.v1) (car k2.v2)))
                                (zip new-kws new-kw-vals)
                                (zip original-kws original-kw-vals))
                    (lambda (k1.v1 k2.v2) (keyword<? (car k1.v1) (car k2.v2)))))
    (keyword-apply fn kws kw-vals reg-args)))

;; round n to k places to the right of the decimal
(define (round-k n k)
  (let ((dec-mover (expt 10 k)))
    (/ (round (* dec-mover n)) dec-mover)))

(define (->string thing)
  (cond ((string? thing) thing)
        ((symbol? thing) (symbol->string thing))
        (else (e "Don't know how to convert '~A' into a string."))))

;; the first invocation of f looks like (f init (first lst))
;; cannot
(define (reduce-right-result kons init lst)
  (if (null? lst)
      init
      (let lp ((lst (rest lst)) (acc (kons (first lst) init)))
        (if (null? lst)
            acc
            (lp (rest lst) (kons (first lst) acc))))))

;;
;; max-f
;;
;; find the elt in lst which has the highest value of (f elt) which is greater than
;; init-max, and return that max value.
;; 
;;
(define (max-f init-max f lst)
  (let ((m init-max))
    (for-each (lambda (elt) (let ((v (f elt))) (when (> v m) (set! m v))))
              lst)
    m))

;;
;; max-f-elt
;;
;; like max-f, but returns the element instead of the max value.
;;
(define (max-f-elt init-max f lst)
  (let ((m init-max)
        (m-elt 'dummy))
    (for-each (lambda (elt) (let ((v (f elt))) (when (> v m) (set! m v) (set! m-elt elt))))
              lst)
    m-elt))

(define (listify x)
  (if (list? x) x (list x)))

(define-syntax sync-on-lock
  (syntax-rules ()
    ((_ lock body ...)
     (begin (semaphore-wait lock)
            (let ((val (begin body ...)))
              (semaphore-post lock)
              val)))))

(define (make-lock)
  (make-semaphore 1))

(define (md5-string str)
  (bytes->string/utf-8 (md5 (string->bytes/utf-8 str))))

;; pretty printing:
(print-hash-table #t)
(print-struct #t)

