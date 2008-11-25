#lang scheme/base

(require "time.scm"
         "util.scm"
         "contract-lp.ss"
         "web-support.scm"
         "settings.scm"
         "page.scm"
         (planet "uuid-v4.ss" ("zitterbewegung" "uuid-v4.plt" 1 0)))

(provide  atom-item
          atom-feed

          rss-feed
          ;; rss-item (via contract)
          )

(define (atom-feed feed-title
                   feed-subtitle
                   feed-url
                   url
                   author-name
                   author-email
                   . body)
  (list-response #:type #"text/xml"
                 (list (raw-str "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
                       `(feed ((xmlns "http://www.w3.org/2005/Atom"))
                              (title ,feed-title)
                              (subtitle ,feed-subtitle)
                              (link ((href ,feed-url) (rel "self")))
                              (link ((href ,url)))
                              (updated ,(atom-time-str (current-seconds))" ")
                              (author (name ,author-name)
                                      (email ,author-email))
                              (id ,(urn)) ,@body))))

(define (atom-item item-title item-link item-summary item-content)
  `(entry
    (title ,item-title)
    (link ((href ,item-link) (rel "self")))
    (id ,(urn))
    (updated ,(atom-time-str (current-seconds)))
    (summary ,item-summary)
    (content ,item-content)))

;;
;; rss-inc
;;
;; Function to include the browser feed auto-discovery link in your page.
;;
(define (rss-inc feed-url)
  `(link ((href ,feed-url) (rel "alternate") (type "application/rss+xml")
          (title "Sitewide RSS Feed"))))

;;
;; rss-feed
;;
;; Generate an RSS 1.0 feed.
;;
;(provide/contract
; (rss-feed ))
;;
(define (rss-feed rss-feed-page
                  #:feed-title feed-title
                  #:feed-description feed-description
                  #:original-content-link (original-content-link (setting *WEB_APP_URL*))
                  #:items (rss-items '()))
  (list-response #:type #"text/xml"
                 (list (raw-str "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
                       `(rdf:RDF
                         ((xmlns "http://purl.org/rss/1.0/")
                          (xmlns:rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
                         (channel ((rdf:about ,(page-url rss-feed-page)))
                                  (title ,feed-title)
                                  (link ,original-content-link)
                                  (description ,feed-description)
                                  (items (rdf:Seq  ,@(map rss-li rss-items))))
                         ,@(map markup-rss-item rss-items)))))

(define (rss-li rss-item)
  `(rdf:li ((resource ,(rss-item-url rss-item)))))

(define-struct rss-item (title url description))

(provide/contract
 (rename construct-rss-item rss-item (->* (#:title string? #:url string?)
                                          (#:description (or/c #f string?))
                                          rss-item?)))
(define (construct-rss-item #:title title #:url url #:description (desc #f))
  (make-rss-item title url desc))

(define (markup-rss-item rss-item)
  (let ((url (rss-item-url rss-item)))
    `(item ((rdf:about ,url))
           (title ,(rss-item-title rss-item))
           (link ,url)
           ,@(splice-if (aand (rss-item-description rss-item) `(description ,it))))))
