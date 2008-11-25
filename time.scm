#lang scheme/base

(require "util.scm"
         "record.scm"
         (planet "rfc3339.ss" ("neil" "rfc3339.plt" 1 0)))

(provide created-when
         created-when-str
         atom-time-str
         days-since
         hours-since
         minutes-since
         seconds->time-string
         A_DAY
         AN_HOUR
         THIRTY_DAYS)

;; returns #f or seconds
(define (created-when rec #:timestamp-key (timestamp-key 'created-at))
  (rec-prop rec timestamp-key))

;; returns "" if no creation time of given rec is known
(define (created-when-str rec #:timestamp-key (timestamp-key 'created-at))
  (aif (created-when rec #:timestamp-key timestamp-key)
       (let* ((now (current-seconds))
              (mins (minutes-since it #:now now))
              (hours (hours-since it #:now now))
              (days (days-since it #:now now)))
         (cond ((= mins 1) (format "~A minute ago" mins))
               ((< mins 60) (format "~A minutes ago" mins))
               ((= hours 1) (format "~A hour ago" hours))
               ((< hours 24) (format "~A hours ago" hours))
               ((= days 1) (format "~A day ago" days))
               (else (format "~A days ago" days))))
       ""))

(define (atom-time-str secs)
  (let ((d (seconds->date secs)))
    (rfc3339-record->string
     (make-rfc3339-record (date-year d) (date-month d) (date-day d)
                          (date-hour d) (date-minute d) (date-second d)
                          #f (/ (date-time-zone-offset d) 60)))))

(define (days-since sec-stamp  #:now (now (current-seconds)))
  (round (/ (- now sec-stamp) A_DAY)))

(define (hours-since sec-stamp #:now (now (current-seconds)))
  (round (/ (- now sec-stamp) AN_HOUR)))

(define (minutes-since sec-stamp #:now (now (current-seconds)))
  (round (/ (- now sec-stamp) 60)))

(define A_DAY 86400)
(define AN_HOUR 3600)
(define THIRTY_DAYS 2592000)

(define (seconds->time-string seconds)
  (let ((d (seconds->date seconds)))
    (format "~A/~A/~A ~A:~A:~A"
            (date-month d)
            (date-day d)
            (date-year d)
            (date-hour d)
            (date-minute d)
            (date-second d))))