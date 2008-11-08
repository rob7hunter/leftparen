#lang scheme/base
(require "util.scm"
         "web-support.scm")

(provide rss-inc
         rss-wrapper
         rss-item
         rss-li)

;;Rss autodiscovery feed include.
(define (rss-inc feed-url)
  `(link (href ,feed-url) (rel "alternate") (type "application/rss+xml")
          (title "Sitewide RSS Feed")))

;;Main wrapper function for rss 1.0
(define (rss-wrapper about
                     channel-title
                     channel-link
                     channel-description
                     channel-image
                     item-list
                     . body)
 (list-response #:type #"text/xml"
                 (list (raw-str "<?xml version=\"1.0\"?>")
                       `(rdf:RDF
                         ((xmlns "http://purl.org/rss/1.0/")
                          (xmlns:rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
                         (channel
                          ((rdf:about ,about))
                          (title ,channel-title)
                          (link ,channel-link)
                          (description
                          
                           ,channel-description)
                          (image ((rdf:resource ,channel-image)))
                          (items 
                                 (rdf:Seq  
                                  ,@item-list)))
                         ,@body))))
;;Rss list creator 
(define (rss-li resource-link)
  `(rdf:li ((resource ,resource-link))))

;;Creation of rss items
(define (rss-item rdf-about item-title item-link item-description)
  `(item
     ((rdf:about ,rdf-about))
     (title () ,item-title)
     (link () ,item-link)
     (description () ,item-description)))
;;Creation of rss textinput items
(define (rss-textinput about text-title text-description text-name text-link)
  `(textinput
    ((rdf:about ,about))
    (title () ,text-title)
    (description () ,text-description)
    (name () , text-name)
    (link () ,text-link)))
