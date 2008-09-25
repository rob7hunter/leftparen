#lang scheme/base

(provide server-log)

(define (server-log format-str . args)
  (display (apply format (string-append format-str "\n") args)))
