#lang scheme/base

(require "time.scm"
         "util.scm"
         "contract-lp.ss"
         "web-support.scm"
         "settings.scm"
         "page.scm"
         (planet "web.scm" ("soegaard" "web.plt" 2 (= 1)))
         (planet "uuid-v4.ss" ("zitterbewegung" "uuid-v4.plt" 1 0)))

(provide ;; atom-feed (via contract)
         ;; atom-item (via contract)

         ;; rss-feed (via contract)
         ;; rss-item (via contract)
         )

(define-struct atom-item (title url updated content))
(define-struct rss-item (title url content))

;;
;; atom-feed
;;
;; Generate an Atom 1.0 feed.
;;
(provide/contract
 (atom-feed (->* (page? #:feed-title string? #:feed-updated/epoch-seconds integer?
                  #:author-name string?)
                 (#:feed-description (or/c #f string?) #:feed-id string?
                  #:related-content-link string? #:items (listof atom-item?))
                 response/full?)))
;;
(define (atom-feed atom-feed-page
                   #:feed-title feed-title
                   #:feed-updated/epoch-seconds feed-updated
                   #:author-name author-name
                   #:feed-description (feed-description #f)
                   #:feed-id (feed-id (page-url atom-feed-page #:absolute #t))
                   #:related-content-link (related-content-link (setting *WEB_APP_URL*))
                   #:items (atom-items '()))
  (list-response #:type #"text/xml"
                 (list (raw-str "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
                       `(feed ((xmlns "http://www.w3.org/2005/Atom"))
                              (id ,feed-id)
                              (title ,feed-title)
                              ,@(splice-if feed-description `(subtitle ,feed-description))
                              (link ((href ,(page-url atom-feed-page #:absolute #t))
                                     (rel "self")))
                              (link ((href ,related-content-link) (rel "alternate")))
                              (updated ,(atom-time-str feed-updated))
                              (author (name ,author-name))
                              ,@(map atom-item-markup atom-items)))))

;;
;; atom-item
;;
;; A way to make atom-items (which are passed into #:items of atom-feed).
;;
(provide/contract
 (rename construct-atom-item atom-item (->* (#:title string? #:url string?
                                             #:updated-epoch-seconds integer?)
                                            (#:content (or/c #f string?))
                                            atom-item?)))
;;
(define (construct-atom-item #:title title #:url url #:updated-epoch-seconds updated
                             #:content (content #f))
  (make-atom-item title url updated content))
  
(define (atom-item-markup atom-item)
  (let ((url (atom-item-url atom-item)))
    `(entry (title ,(atom-item-title atom-item))
            (link ((href ,url) (rel "self")))
            (id ,url)
            (updated ,(atom-time-str (atom-item-updated atom-item)))
            ,@(splice-if (aand (atom-item-content atom-item) `(content ,it))))))

;;
;; rss-feed
;;
;; Generate an RSS 1.0 feed.
;;
(provide/contract
 (rss-feed (->* (page? #:feed-title string? #:feed-description string?)
                (#:related-content-link string? #:items (listof rss-item?))
                response/full?)))
;;
(define (rss-feed rss-feed-page
                  #:feed-title feed-title
                  #:feed-description feed-description
                  #:related-content-link (related-content-link (setting *WEB_APP_URL*))
                  #:items (rss-items '()))
  (list-response #:type #"text/xml"
                 (list (raw-str "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
                       `(rdf:RDF
                         ((xmlns "http://purl.org/rss/1.0/")
                          (xmlns:rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
                         (channel ((rdf:about ,(page-url rss-feed-page #:absolute #t)))
                                  (title ,feed-title)
                                  (link ,related-content-link)
                                  (description ,feed-description)
                                  (items (rdf:Seq ,@(map rss-li rss-items))))
                         ,@(map rss-item-markup rss-items)))))

(define (rss-li rss-item)
  `(rdf:li ((resource ,(rss-item-url rss-item)))))

;;
;; rss-item
;;
;; A way to make rss-items (which are passed into #:items of rss-feed)
;;
(provide/contract
 (rename construct-rss-item rss-item (->* (#:title string? #:url string?)
                                          (#:content (or/c #f string?))
                                          rss-item?)))
;;
(define (construct-rss-item #:title title #:url url #:content (content #f))
  (make-rss-item title url content))

(define (rss-item-markup rss-item)
  (let ((url (rss-item-url rss-item)))
    `(item ((rdf:about ,url))
           (title ,(rss-item-title rss-item))
           (link ,url)
           ,@(splice-if (aand (rss-item-content rss-item) `(description ,it))))))
