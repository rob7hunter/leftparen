#lang scheme/base

(require "time.scm"
         "util.scm"
         "web-support.scm"
         (planet "uuid-v4.ss" ("zitterbewegung" "uuid-v4.plt" 1 0)))

(provide  atom-item
          atom-inc
          atom-wrapper)

(define (atom-inc feed-url)
  `(link ((rel "alternate") (type "application/atom+xml") (href ,feed-url))))

(define (atom-wrapper feed-title
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
