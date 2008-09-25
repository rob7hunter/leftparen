;; how much code have you written?
#lang scheme/base

(require "util.scm")

(provide loc)

;; counts all lines except for comment lines and blank lines
(define (loc #:comment-chars (comment-chars (list #\;)) . filenames)
  (fold + 0
        (map (lambda (filename)
               (file-line-fold
                (lambda (line-str total-loc)
                  (let ((trimmed (string-trim-both line-str #\space)))
                    (cond ((string=? trimmed "") total-loc)
                          ((memq (string-ref trimmed 0) comment-chars) total-loc)
                          (else (+ 1 total-loc)))))
                0
                filename))
             filenames)))


