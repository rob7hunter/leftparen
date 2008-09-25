#lang scheme/base

;; a list of all the library forms

(provide lib-bindings)

(define
  lib-bindings
  '(
    ;; request:
    req
    req-set!
    
    ;; server:
    server-start
    server-port
    server-listen-ip
    server-htdocs-path
    
    gen-page
    page-wrapper
    
    ))
