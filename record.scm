#lang scheme/base

(require (file "util.scm"))

(provide r
         make-rec
         rec-id
         rec-data
         rec-set-data!
         rec-set-rec-prop!
         rec-prop
         rec-multi-props
         rec-has-prop?
         rec-set-prop!
         rec-remove-prop!
         rec-set-each-prop!
         rec-intern-prop!
         rec-type-is?
         rec-type
         rec-add-list-prop-elt!
         rec-add-child!
         rec-remove-child!
         rec-child-prop
         rec-pretty-string
         rec-filter-where
         rec?
         same-rec?
         id-is?
         )

(define-struct record-obj (data id) #:mutable)

(define rec? record-obj?)

(define-syntax r
  (syntax-rules (:)
    ((_ (key : val) ...)
     (new-rec (list (cons `key val) ...)))))

(define (rec-id r)
  (record-obj-id r))

(define (rec-data r)
  (record-obj-data r))

(define (rec-set-data! r new-data)
  (set-record-obj-data! r new-data)
  r)

(define (rec-type-is? r type)
  (eq? type (rec-type r)))

(define (rec-type r)
  (rec-prop r 'type))

;; a data is ((KEY . VAL) ...)

(define (make-rec data id)
  (make-record-obj data id))

(define (rec-has-prop? r prop)
  (and (assoc prop (rec-data r)) #t))

(define (rec-set-prop! r prop val)
  (set-record-obj-data! r
                        (cons (cons prop val)
                              (removef (match-lambda ((list-rest k v) (eq? k prop)))
                                       (rec-data r)))))

(define (rec-remove-prop! r . props)
  (set-record-obj-data! r
                        (removef (match-lambda ((list-rest k v) (memq k props)))
                                 (rec-data r))))

(define (rec-add-list-prop-elt! r list-prop new-elt)
  (rec-set-prop! r list-prop (cons new-elt (rec-prop r list-prop '()))))

(define (rec-set-each-prop! r prop.=>val)
  (for-each (match-lambda ((list-rest prop val) (rec-set-prop! r prop val)))
            prop.=>val)
  r)

(define (rec-intern-prop! r prop)
  (rec-set-prop! r prop (string->symbol (rec-prop r prop))))

;; if #:to-end is non-#f, then add the child to the end of the child list.
(define (rec-add-child! parent prop child #:to-end (to-end #f))
  (rec-set-prop! parent prop
                 ((if to-end cons-to-end cons)
                  (rec-id child) (rec-child-prop parent prop))))

;; does nothing to the child itself ... only the parent's pointer to the child
(define (rec-remove-child! parent prop child)
  (rec-set-prop! parent prop (delete (rec-id child) (rec-child-prop parent prop))))

(define (rec-child-prop parent prop)
  (rec-prop parent prop '()))

(define (rec-set-rec-prop! rec prop other-rec)
  (rec-set-prop! rec prop (rec-id other-rec)))

(define (rec-prop rec prop-name (missing-value #f))
  (let ((result (assoc prop-name (rec-data rec))))
    (if result
        (cdr result)
        missing-value)))

(define (rec-multi-props rec props)
  (map (cut rec-prop rec <>) props))

(define (rec-pretty-string rec)
  (format "~A : ~A" (rec-id rec) (map (lambda (k.v) `(,(car k.v) : ,(cdr k.v)))
                                      (rec-data rec))))

(define (rec-filter-where rec-lst pairs #:equal-fn (equal-fn equal?))
  (let ((results '()))
    (for-each (lambda (r)
                (when (every (match-lambda ((list-rest prop val)
                                            ;; XXX do proper numeric equality
                                            ;; here too!
                                            (equal-fn val (rec-prop r prop))))
                             pairs)
                  (set! results (cons r results))))
              rec-lst)
    results))

;; does an id comparison
(define (same-rec? r1 r2)
  (string=? (rec-id r1) (rec-id r2)))

(define (id-is? rec some-id)
  (string=? (rec-id rec) some-id))
