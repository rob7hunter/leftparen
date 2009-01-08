#lang scheme/base

;; A simple re-exporting of common web-related PLT tools.  This is meant to be used
;; internally by LeftParen source code, though I suppose you could require it in your
;; own LeftParen projects if you want.

(require web-server/http/request-structs
         web-server/http/response-structs
         web-server/http/bindings
         web-server/http/redirect
         net/url)

(provide make-header
         header?
         
         make-response/full
         response?
         response/full?
         response/incremental?
         response/basic?

         binding:form?
         binding-id
         binding:form-value
         binding:file-filename
         binding:file-content
         exists-binding?
         extract-binding/single

         request?
         request-bindings/raw
         request-bindings
         request-post-data/raw
         request-uri
         request-headers
         
         redirect-to

         url->string
         
         )

