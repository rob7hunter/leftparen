#lang scheme/base

(require (file "util.scm")
         (file "record.scm")
         (file "settings.scm")
         "log.scm")

(provide store-rec!
         delete-rec!
         add-child-and-save!
         remove-child-and-save!
         load-children
         load-descendants
         contains-child?
         load-rec
         record-id-stored?
         refresh
         rec-rec-prop
         load-where
         load-one-where
         fresh-rec-from-data
         only-rec-of-type
         is-descendant?
         find-parent
         find-ancestor
         find-highest-ancestor
         find-incoming-record
         find-incoming-records
         sort-recs-by
         define-cache
         define-type-cache
         populate-caches
         )

;;
;; Usage, e.g.:
;;  (only-rec-of-type passed-in-rec-id location (item)
;;                    (rec-prop item 'title))
;;
(define-syntax only-rec-of-type
  (syntax-rules ()
    ((_ rec-id type (rec-iden) body ...)
     (let ((rec-iden (load-rec rec-id)))
       (if (and rec-iden (rec-type-is? rec-iden 'type))
           (begin body ...)
          (e "Unauthorized access."))))))

(define (store-rec! r)
  (cache-store r)
  (write-record! r)
  r)

;; if recur is #t, then look for child-props in the children too
(define (delete-rec! r
                     #:children-props-to-delete (child-props '())
                     #:recur (recur #f))
  (define (delete-chillins item)
    (for-each (lambda (child-prop)
                (for-each (lambda (child-id)
                            (let ((child (load-rec child-id)))
                              (cache-delete child)
                              (delete-file (abs-path-to-record child-id))
                              (when recur (delete-chillins child))))
                          (rec-child-prop item child-prop)))
              child-props))
  (cache-delete r)
  (delete-file (abs-path-to-record (rec-id r)))
  (delete-chillins r))

;; pass #:ensure if you want to make sure that the record you are loading has particular
;; properties.  A common use case is a security check so user can't url hack to load
;; arbitrary records: (load-rec some-id #:ensure '((type . blog-post))
;;
;; It is an error if the given record id doesn't exist.
;;
(define (load-rec id #:ensure (ensure '()))
  (or (and-let* ((data (read-record-data id))
                 (result (rec-filter-where (list (make-rec data id)) ensure)))
        (if (empty? result)
            #f
            (first result)))
      (e "Record with id ~A does not exist in repository." id)))

(define (refresh rec)
  (load-rec (rec-id rec)))
 
;; adds the given child to the parent and saves the parent (not the child).
;; if #:to-end is non-#f, then add the child to the end of the child list.
(define (add-child-and-save! parent prop child #:to-end (to-end #f))
  (rec-add-child! parent prop child #:to-end to-end)
  (store-rec! parent))

(define (remove-child-and-save! parent prop child)
  (rec-remove-child! parent prop child)
  (store-rec! parent))

(define (load-children parent prop)
  (map load-rec (rec-child-prop parent prop)))

(define (load-descendants parent recursive-prop)
  (let ((one-level (load-children parent recursive-prop)))
    (append one-level (append-map (cut load-descendants <> recursive-prop) one-level))))

;; returns #f if nothing exists for the property.
(define (rec-rec-prop rec prop)
  (aand (rec-prop rec prop) (load-rec it)))

(define (contains-child? parent prop putative-child)
  (any (cute string=? <> (rec-id putative-child)) (rec-child-prop parent prop)))

;; if id is already provided then don't overwrite
(define (fresh-rec-from-data data
                             #:stamp-time (stamp-time #f)
                             #:id-length (id-length #f)
                             #:id (id #f))
  (let* ((id (or (assoc-val 'id data)
                 (cond (id id)
                       (id-length (fresh-id #:id-length id-length))
                       (else (fresh-id)))))
         (rec (make-rec data id)))
    (when stamp-time (rec-set-prop! rec 'created-at (current-seconds)))
    rec))

(define (fresh-id #:id-length (id-length 5))
  (let ((try (random-key-string id-length)))
    (if (file-exists? (abs-path-to-record try))
        (fresh-id #:id-length id-length)
        try)))

(define (ignore-filename? filename-path)
  (let ((ignore-regexps
         (list
          ;; subversion directories:
          ".svn"
          ;; emacs tmp files:
          "~$")))
    (and (any (lambda (re) (pregexp-match re filename-path)) ignore-regexps) #t)))

;;
;; load-all-recs
;;
;; #:type should be a symbol or a list of symbols
;;
;; this references the type caches for potential efficiency improvements.
;;
(define (load-all-recs #:type (type-or-types #f))
  (or (and type-or-types
           (if (list? type-or-types)
               (let ((lookups (map type-cache-get-records type-or-types)))
                 (if (any not lookups)
                     #f
                     lookups))
               (type-cache-get-records type-or-types)))
      (filter-map (lambda (filename-path)
                    (let ((filename (path->string filename-path)))
                      (and (not (ignore-filename? filename))
                           (and-let* ((rec (load-rec filename)))
                             (cond ((not type-or-types) rec)
                                   ((symbol? type-or-types)
                                    (and (rec-type-is? rec type-or-types) rec))
                                   ((list? type-or-types)
                                    (and (memq (rec-prop rec 'type) type-or-types) rec))
                                   (else (e "Can't restrict where type(s) is '~A'."
                                            type-or-types)))))))
                  (directory-list (setting *PATH_TO_DATA*)))))
  
;; uses AND logic
;; restricted-to should be a list of record ids if given or a predicate.
;; - sort-by is either a symbol (indicating a record property) or a function on a record
;;   that returns a value.
(define (load-where (pairs '())
                    #:restricted-to (restricted-to #f)
                    #:type (type-or-types #f)
                    #:sort-by (sort-by #f)
                    #:compare (compare <)
                    #:equal-fn (equal-fn equal?) ; used in pair value equality test
                    #:exactly-one (exactly-one #f)
                    #:limit (limit #f))
  (let* ((result
          (rec-filter-where (cond ((and restricted-to (list? restricted-to))
                                   (map load-rec restricted-to))
                                  (restricted-to
                                   ;; then restricted-to is a fn:
                                   (filter restricted-to
                                           (load-all-recs #:type type-or-types)))
                                  (else (load-all-recs #:type type-or-types)))
                            pairs
                            #:equal-fn equal-fn))
         (sorted (sort-recs-by result sort-by #:compare compare)))
    (cond (exactly-one (if (empty? sorted) #f (first sorted)))
          (limit (take-up-to sorted limit))
          (else sorted))))

;; sort-by is either #f,
;; a symbol (indicating a record property),
;; or a function on a record that returns a value.
(define (sort-recs-by recs sort-by #:compare (compare-fn <))
  (if sort-by
      (sort recs (lambda (a b)
                   (if (procedure? sort-by)
                       (compare-fn (sort-by a) (sort-by b))
                       (compare-fn (rec-prop a sort-by)
                                   (rec-prop b sort-by)))))
      recs))

;; restricted-to should be a list of record ids if given.
(define (load-one-where pairs #:restricted-to (restricted-to #f))
  (let ((results (load-where pairs)))
    (if (empty? results)
        #f
        (first results))))

(declare-setting *PATH_TO_DATA* (build-path (current-directory) "data"))
                 
;; persists the given record on disk, overwriting any previous record of the same id
;; that may have been there.
(define (write-record! r)
  (let ((id (rec-id r)))
    (call-with-output-file (abs-path-to-record id)
      (lambda (port)
        (write (rec-data r) port))
      #:exists 'replace)))

;; returns #f if no such file in data/ directory.
(define (read-record-data id)
  (let ((p (abs-path-to-record id)))
    (and (file-exists? p)
         (call-with-input-file p
           (lambda (port)
             (read port))))))

;; is the given record stored in the repository?
(define (record-stored? rec)
  (record-id-stored? (rec-id rec)))

(define (record-id-stored? rec-id)
  (file-exists? (abs-path-to-record rec-id)))

(define (abs-path-to-record id)
  (unless (directory-exists? (setting *PATH_TO_DATA*))
    (e "Can't find data directory '~A'. Current directory is ~A."
       (setting *PATH_TO_DATA*) (current-directory)))
  (build-path (setting *PATH_TO_DATA*) id))

;;
;; find-parent
;;
(define (find-parent child-rec child-prop #:parent-type (parent-type #f))
  (find-incoming-records child-rec child-prop
                         #:find-type parent-type
                         #:is-ptr-to-list #t
                         #:return-just-one-record #t))

;; is low-rec a (strict) descendant of high-rec?
(define (is-descendant? low-rec high-rec child-ptr-prop)
  (any (cut same-rec? <> low-rec) (load-descendants high-rec child-ptr-prop)))

;; like find-parent, but the starting-rec is not part of a list of children coming out
;; of the incoming (parent) rec.
(define (find-incoming-record starting-rec ptr-prop
                              #:find-type (find-type #f))
  (find-incoming-records starting-rec ptr-prop
                         #:find-type find-type
                         #:return-just-one-record #t))

;;
;; find-incoming-records
;;
;; You have a record, and want to search for records that point into it (aka, incoming
;; records). If you return-just-one-record and there are no incoming records, then this
;; returns #f.
;;
(define (find-incoming-records starting-rec ptr-prop
                               #:find-type (find-type #f)
                               #:is-ptr-to-list (is-ptr-to-list #f)
                               #:return-just-one-record (just-one #f))
  ;; XXX we'd like to have this here, but it will require a touch of work on the cache:
  ;; #:sort-by (sort-by #f)
  ;; #:compare (compare <))
  (let ((clookup (cache-lookup starting-rec find-type ptr-prop)))
    (if clookup
        (cond ((and just-one (not (empty? clookup))) (first clookup))
              (just-one #f)
              (else clookup))
        ;; o/w do it the slow way
        (let ((start-rec-id (rec-id starting-rec))
              (find-fn (if just-one find filter)))
          (find-fn (lambda (outgoing-rec)
                     (if is-ptr-to-list
                         (member start-rec-id (rec-child-prop outgoing-rec ptr-prop))
                         (aand (rec-prop outgoing-rec ptr-prop)
                               (string=? it start-rec-id))))
                   (load-where #:type find-type))))))

;;
;; find-ancestor
;;
;; Note that it is acceptable to return the given descendant-rec if pred is satisfied.
;;
(define (find-ancestor descendant-rec pred recursive-child-prop
                       #:ancestor-type (anc-type #f))
  (if (pred descendant-rec)
      descendant-rec
      (let ((p (find-parent descendant-rec recursive-child-prop #:parent-type anc-type)))
        (and p
             (find-ancestor p pred recursive-child-prop #:ancestor-type anc-type)))))

(define (find-highest-ancestor descendant-rec recursive-child-prop
                              #:ancestor-type (anc-type #f))
  (let ((p (find-parent descendant-rec recursive-child-prop #:parent-type anc-type)))
    (if p
        (find-highest-ancestor p recursive-child-prop #:ancestor-type anc-type)
        descendant-rec)))

(define (robust-path->string str-or-path)
  (if (string? str-or-path)
      str-or-path
      (path->string str-or-path)))

;; Cache stuff

;; load-fn is called at the beginning of time to load a repository into the cache.
;; store-fn is called when a record is stored (either when a record is created or mutated).
;; delete-fn is called when a record is deleted
;; lookup-fn is called to get a value (quickly) out of the cache.
;; ht is the hash table which is the cache proper.
(define-struct cache-spec (load-fn store-fn delete-fn might-answer-fn lookup-fn ht))

(define-struct type-cache (type-name records) #:mutable)
  
(define *CACHE_SPECS* (list))

(define *TYPE_CACHES* (list))

(define (add-to-cache-specs! cs)
  (set! *CACHE_SPECS* (cons cs *CACHE_SPECS*)))

(define (add-to-type-caches! tc)
  (set! *TYPE_CACHES* (cons tc *TYPE_CACHES*)))

;;
;; define-cache
;;
;; When using records, we usually "point" to other records in just one direction.
;; So, for example, each "movie" record might have some "actors" pointing out of it.
;; Thus, we can efficiently ask for the actors in a given movie, but the reverse question
;; ("which movies was this actor in?") is slower since it involves (potentially)
;; looking at each movie to find ones that have the actor.
;;
;; We use easy-to-create caches to make this computation faster.  These caches store
;; the "incoming connections", which, in the above example, would correspond to
;; actors to movies.  We would define a cache to represent this with
;;
;; (define-cache movies-with-this-actor
;;   actors <= (movie))
;;
;; The arrow (<=) indicates the direction of the pointers in the record repository.
;; But to see which way the cache helps you, you just read is left to right (i.e, given
;; an actor, we can find a movie).  The parens around "movie" indicate that the "actors"
;; property is a list property.
;; 
;; All records in the cache are just stored by id, so if you change a property, the old
;; property will *not* be cached...unless of course you are using some other caching
;; scheme in combination with this which does cache record values.
;;
;; This form binds a name to a hash table which maps record ids (of type implied by the
;; prop-ptr) to either a record or list of records of type incoming-type.
;;
;; When you change a record that is pointed to
;;
(define-syntax define-cache
  (syntax-rules (<=)
    ((_ name prop-ptr <= (incoming-type))
     (begin (define name (make-hash))
            (add-to-cache-specs! (create-cache-spec name 'prop-ptr 'incoming-type
                                                    #:has-many-incoming #t))))
    ((_ name prop-ptr <= incoming-type)
     (begin (define name (make-hash))
            (add-to-cache-specs! (create-cache-spec name 'prop-ptr 'incoming-type))))))

;;
;; define-type-cache
;;
;; type caches give you fast access to records of a particular type.
;;
;; Example usage:
;; (define-type-cache movie)
;;
(define-syntax define-type-cache
  (syntax-rules ()
    ((_ type)
     (add-to-type-caches! (make-type-cache 'type (list))))))

(define (create-cache-spec ht prop-ptr incoming-type
                           ;; XXX we aren't using has-many-incoming yet
                           #:has-many-incoming (has-many-incoming #f))
  
  (define (add! incoming-rec points-to-id)
    (hash-set! ht points-to-id (cons (rec-id incoming-rec)
                                     (hash-ref ht points-to-id '()))))

  (define (remove! incoming-rec formerly-pointed-to-id)
    (hash-set! ht formerly-pointed-to-id (removef (cute string=? <> (rec-id incoming-rec))
                                                  (hash-ref ht formerly-pointed-to-id))))

  (define i-care? (cut rec-type-is? <> incoming-type))
  
  (define (load-fn incoming-rec)
    (when (i-care? incoming-rec)
      (awhen (rec-prop incoming-rec prop-ptr)
        (for-each (cut add! incoming-rec <>) (listify it)))))

  ;; this should be called before the store actually happens so we can get access to the
  ;; "pre-store" data
  (define (store-fn incoming-rec)
    (when (i-care? incoming-rec)
      (let ((old-ptrs (and (record-stored? incoming-rec)
                           (rec-prop (load-rec (rec-id incoming-rec)) prop-ptr)))
            (new-ptrs (rec-prop incoming-rec prop-ptr)))
        
        (unless (equal? old-ptrs new-ptrs)
          (when old-ptrs
            (for-each (cut remove! incoming-rec <>) (listify old-ptrs)))
          (load-fn incoming-rec)))))

  (define (delete-fn incoming-rec)
    (when (i-care? incoming-rec)
      (awhen (rec-prop incoming-rec prop-ptr)
        (for-each (cut remove! incoming-rec <>) (listify it)))))

  (define (might-answer-fn a-incoming-type a-prop-ptr)
    (and (eq? incoming-type a-incoming-type)
         (eq? prop-ptr a-prop-ptr)))

  ;; only call this if might-answer? is #t
  (define (lookup-fn a-rec a-incoming-type a-prop-ptr)
    (hash-ref ht (rec-id a-rec) '()))

  (make-cache-spec load-fn store-fn delete-fn might-answer-fn lookup-fn ht))

;; Basic cache ops follow.  these are used for both incoming ptr caches, and
;; type caches. In the case of incoming ptr caches, for cache-load, cache-store,
;; cache-delete and cache-lookup, you should consider the given rec as the "incoming rec".

;; this happens when the server is started
(define (cache-load rec)
  ;; incoming ptr cache load:
  (for-each (lambda (spec) ((cache-spec-load-fn spec) rec))
            *CACHE_SPECS*)
  ;; type cache load:
  (add-record-to-type-cache! rec))

(define (cache-store rec)
  ;; store in main caches:
  (for-each (lambda (spec) ((cache-spec-store-fn spec) rec))
            *CACHE_SPECS*)
  ;; store in type cache:
  (add-record-to-type-cache! rec))

(define (cache-delete rec)
  ;; delete from main caches:
  (for-each (lambda (spec) ((cache-spec-delete-fn spec) rec))
            *CACHE_SPECS*)
  ;; delete from type cache:
  (delete-record-from-type-cache! rec))

;; returns #f if no caches apply
(define (cache-lookup starting-rec incoming-type prop-ptr)
  (let ((appro-spec (find (lambda (spec)
                            ((cache-spec-might-answer-fn spec) incoming-type prop-ptr))
                          *CACHE_SPECS*)))
    (and appro-spec
         (map load-rec
              ((cache-spec-lookup-fn appro-spec) starting-rec incoming-type prop-ptr)))))

;; returns #f (if there is no cache to answer this question)
;; or o/w a list of all records of the given type
(define (type-cache-get-records type-name)
  (aand (find-type-cache-with-name type-name)
        (type-cache-records it)))

(define (find-type-cache rec)
  (find-type-cache-with-name (rec-type rec)))

(define (find-type-cache-with-name type-name)
  (find (lambda (type-cache) (eq? type-name (type-cache-type-name type-cache)))
        *TYPE_CACHES*))

;; if the rec is already in the type cache, this replaces it.  This might be a future point
;; of optimization because we aren't being efficient here.
(define (add-record-to-type-cache! rec)
  (let ((id (rec-id rec)))
    (awhen (find-type-cache rec)
      (let ((recs-in-cache (type-cache-records it)))
        (set-type-cache-records! it (cons rec
                                          (if (record-id-stored? id)
                                              ;; xxx: technically we can stop once we
                                              ;; found it.
                                              (removef (cut id-is? <> id) recs-in-cache)
                                              recs-in-cache)))))))

(define (delete-record-from-type-cache! rec)
  (awhen (find-type-cache rec)
    (let ((its-id (rec-id rec)))
      (set-type-cache-records! it (removef (lambda (r) (string=? (rec-id r) its-id))
                                           (type-cache-records it))))))

(define (populate-caches)
  (server-log "Populating caches...")
  (for-each cache-load (load-all-recs))
  (server-log "Done populating cache."))

