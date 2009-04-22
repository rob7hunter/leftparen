#lang scheme/base

(require net/url
         (planet untyped/dispatch:2:=1/dispatch)
         "util.scm"
         "web-export.ss"
         "web-support.scm"
         "session.scm"
         "settings.scm"
         "time.scm")

(provide define-page
         define-session-page
         page
         page?
         design
         **
         page-url
         redirect-to-page
         js-inc
         css-inc
         versioned-file-reference
)

;;
;; define-page
;;
;; e.g., (define-page (foo-page req)
;;         #:css '("/css/main.css")
;;         #:js  '("/js/foo.js" "/js/bar.js")
;;         "Hello, World!")
;;
(define-syntax define-page
  (syntax-rules ()
    ((_ (page-name args ...)
        keywords-and-body ...)
     (define-controller (page-name args ...)
       (page keywords-and-body ...)))))

(define-syntax define-session-page
  (syntax-rules ()
    ((_ (page-name req-iden sesh-iden args ...)
        keywords-and-body ...)
     (define-controller (page-name req-iden args ...)
       (sessioned-response req-iden (sesh-iden)
                           (page keywords-and-body ...))))))

;;
;; page
;;
;; * if #:blank is non-#f, then all other keyword args are ignored and a blank page is
;; returned containing the given bodies.
;; * if #:design is given (and #:blank is not), then all other keywords are ignored.
;;   the value passed for #:design should be the result of a (design ...) invocation.
;;   Really, it's just a "page" call, abstracted out so it can be reused on potentially
;;   many different pages.
;; * #:raw-header is a list of strings which are inserted directly at the beginning of the
;;   <head /> area of the page.
;; * #:doc-type should be #f or a str.  If str, it is automatically "rawed".
;; * #:redirect-to should be #f or a URI str.  If str, the body is evaluated but not
;;   returned (since you are asking to redirect).
;; 
(define (page #:doc-type (doc-type #f)
              #:raw-header (raw-header '())
              #:css (css '())
              #:js (js '())
              #:atom-feed-page (atom-feed-page #f)
              #:rss-feed-page (rss-feed-page #f)
              #:title (title "a LeftParen web app")
              #:body-attrs (body-attrs '())
              #:body-wrap (body-wrap (lambda (body) body))
              #:blank (blank #f)
              #:design (a-design #f)
              #:redirect-to (redirect-to #f)
              . body)
  (let ((returned-body
         (if (empty? body)
             (if (not redirect-to)
                 (e "Unless you are doing a #:redirect-to, a body is required.")
                 #f)
             (body-wrap (last body)))))
    (cond (redirect-to (response-promise-to-redirect redirect-to))
          ((response/full? returned-body) returned-body)
          ((response-promise? returned-body) returned-body)
          (blank returned-body) ; the type of response is default (text/html)
          (a-design (a-design returned-body))
          (else (let ((main `(html (head ,@(map css-inc css)
                                         ,@(splice-if atom-feed-page
                                                      (atom-inc (page-url atom-feed-page)))
                                         ,@(splice-if rss-feed-page
                                                      (rss-inc (page-url rss-feed-page)))
                                         ,@(map js-inc js)
                                         ,@(map raw-str raw-header)
                                         (title ,title))
                                   (body ,body-attrs ,returned-body))))
                  (if doc-type
                      `(group ,(raw-str doc-type) ,main)
                      main))))))

;;
;; design
;;
;; for abstracting out page designs (e.g., headers, body wrappers, etc)
;;
;; MMM I don't particularly like this (semi) duplication of "page"'s keyword args
;;
(define (design #:raw-header (raw-header '())
                #:css (css '())
                #:js (js '())
                #:title (title "a LeftParen web app")
                #:atom-feed-page (atom-feed-page #f)
                #:rss-feed-page (rss-feed-page #f)
                #:doc-type (doc-type #f) ; automatically "rawed" for you
                #:body-attrs (body-attrs '())
                #:body-wrap (body-wrap (lambda (body) body)))
  (lambda (body) (page #:doc-type doc-type
                       #:raw-header raw-header
                       #:css css
                       #:atom-feed-page atom-feed-page
                       #:rss-feed-page rss-feed-page
                       #:js js
                       #:title title
                       #:body-attrs body-attrs
                       #:body-wrap body-wrap
                       body)))

;;
;; ** (the multi-xexpr function)
;;
;; for providing more than one xexpr. e.g,
;;  (** "Hello, world!"
;;      (let ((x 10))
;;        `(em ,(format "~A + ~A = 20!" x x)))
;;      `(strong "the end"))
;;  => 
;;   Hello World
;;   <em>10 + 10 = 20!</em>
;;   <strong>the end</strong>
;;
(define (** . bodies)
  `(group ,@bodies))

(define (js-inc script-filename)
  `(script ((src ,script-filename) (type "text/javascript")) "")) 

(define (css-inc css-filename)
  `(link ((rel "stylesheet") (type "text/css") (href ,css-filename))))

(define (atom-inc feed-url)
  `(link ((rel "alternate") (type "application/atom+xml") (href ,feed-url))))

(define (rss-inc feed-url #:title (title "RSS feed"))
  `(link ((href ,feed-url) (rel "alternate") (type "application/rss+xml")
          (title ,title))))

;; filename should be relative to htdocs directory
;; XXX I'm not sure this will actually work (does the # trigger a new file refresh?)
;; XXX INDEED IT DOES NOT.  We'll have to change the actual filename and then
;; use a symlink.
(define (versioned-file-reference filename)
  (string-append filename "#" (number->string (setting *APP_VERSION*))))

(define (redirect-to-page page-name . args)
  (redirect-to (apply controller-url page-name args)))

(define (page-url page #:absolute (absolute #f) . page-args)
  (let ((rel-url (apply controller-url page page-args)))
    (if absolute
        (url->string (combine-url/relative (string->url (setting *WEB_APP_URL*))
                                           rel-url))
        rel-url)))

(define page? controller?)
