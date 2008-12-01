#lang scheme/base

;;
;; code to generate basic files and folders needed for a project
;;

(require "util.scm"
         (only-in mzlib/file normalize-path))

(provide generate generate-from-path)

(define PLANET_MAJOR_VERISON 3)
(define PLANET_MINOR_VERSION 0)

(define (generate args-vec)
  (generate-from-args-list (vector->list args-vec)))

(define (generate-from-path project-path args-vec)
  (generate-from-args-list (cons-to-end project-path (vector->list args-vec))))

;; Note: in order for this to work with generate-from-path, you need to make sure that
;; for every command, the last argument is always the project path.
(define (generate-from-args-list args)
  (match args
         ((list "project" fresh-project-path)
          (generate-project-dir fresh-project-path))
         ((list "script" project-path)
          (generate-script-dir project-path))
         ((list "scm" project-path)
          (generate-basic-scm-files project-path))
         ((list project-path)
          (e "You need to provide a command to the generate script."))
         ((list-rest command rst)
          (e "Generate command \"~A\" not understood." command))
         (else (e "Generate expression \"generate ~A\" not understood." args))))

(define (generate-project-dir fresh-project-path)
  (if (directory-exists? fresh-project-path)
      (e "The directory ~A already exists." fresh-project-path)
      (begin (ensure-existence-of-dir! fresh-project-path)
             (ensure-existence-of-dir! (build-path fresh-project-path "data"))
             (ensure-existence-of-dir! (build-path fresh-project-path "uploaded-files"))
             (ensure-existence-of-dir! (build-path fresh-project-path "htdocs"))
             (ensure-existence-of-dir! (build-path fresh-project-path "htdocs/css"))
             (ensure-existence-of-dir! (build-path fresh-project-path "htdocs/js"))
             (ensure-existence-of-dir! (build-path fresh-project-path "htdocs/i"))
             (generate-basic-scm-files fresh-project-path)
             (generate-script-dir fresh-project-path))))

(define (generate-basic-scm-files project-path)
  ;; serve.scm
  (generate-file-with-expressions
   #:dir-must-exist #t
   (build-path project-path "serve.scm")
   `(require ,(expr-for-lp-require "leftparen.scm")
             "app.scm"
             "main.scm")
   (make-raw "")
   '(load-server-settings)
   (make-raw "")
   '(serve my-app
           #:listen-ip (setting *LISTEN_IP*)
           #:port (setting *PORT*)
           #:htdocs-path '("htdocs"))
   )

  ;; app.scm
  (generate-file-with-expressions
   #:dir-must-exist #t
   (build-path project-path "app.scm")
   (make-raw "#lang scheme/base\n")
   `(require ,(expr-for-lp-require "leftparen.scm"))
   (make-raw "")
   '(define-app my-app
      (index-page (url "/")))
   )
   
  ;; main.scm
  (generate-file-with-expressions
   #:dir-must-exist #t
   (build-path project-path "main.scm")
   (make-raw "#lang scheme/base\n")
   `(require ,(expr-for-lp-require "leftparen.scm")
             "app.scm")
   (make-raw "")
   '(define-page (index-page req)
      "Hello, World!")
   )

  ;; settings-localhost.scm
  (generate-file-with-expressions
   #:dir-must-exist #t
   (build-path project-path "settings-localhost.scm")
   `(require ,(expr-for-lp-require "settings.scm"))
   (make-raw "")
   '(setting-set! *PORT* 8765)
   (make-raw ";; use #f if you want to listen to all incoming IPs:")
   '(setting-set! *LISTEN_IP* "127.0.0.1")
   '(setting-set! *WEB_APP_URL* "http://localhost:8765/")
   )
  
  )

(define (generate-script-dir project-path)
  (ensure-existence-of-dir! project-path #:must-previously-exist #t)
  ;; script/server
  (generate-file-with-expressions
   (build-path project-path "script/server")
   ;; we double-quote the executable name in case it's a path with, e.g., spaces:
   (make-raw (format "\"~A\" -r serve.scm $1" (find-system-path 'exec-file))))
  
  )

(define-struct raw (str))

(define (generate-file-with-expressions path-to-file
                                        #:dir-must-exist (dir-must-exist #f)
                                        . expressions)
  (with-output-to-file-in-dir
   #:must-previously-exist dir-must-exist
   path-to-file
   (lambda ()
     (for-each (lambda (e)
                 (if (raw? e) (write-string (raw-str e)) (write e))
                 (write-string "\n")) expressions)
     #t)))

(define (ensure-existence-of-dir! dir-path #:must-previously-exist (must-exist #f))
  (when (file-exists? dir-path)
    (e "A file called ~A instead of a directory was found." dir-path))
  (if must-exist
      (if (directory-exists? dir-path)
          #t
          (e "The directory ~A cannot be found." dir-path))
      (or (directory-exists? dir-path)
          (begin (make-directory dir-path)
                 (display (format "Created directory ~A.\n" dir-path))))))

;; it's an error if filename in path already exists
(define (with-output-to-file-in-dir path-to-file thunk
                                    #:must-previously-exist (must-exist #f))
  (receive (path filename is-dir) (split-path path-to-file)
    (ensure-existence-of-dir! path #:must-previously-exist must-exist)
    (with-output-to-file (build-path path filename) thunk #:mode 'text #:exists 'error)))

(define (expr-for-lp-require filename-rel-to-lib-root)
  `(planet ,filename-rel-to-lib-root ("vegashacker" "leftparen.plt"
                                      ,PLANET_MAJOR_VERISON ,PLANET_MINOR_VERSION)))

;; consumes a (maybe relative) project-path
;; XXX rid?
;;(define (absolute-project-path-str project-path)
;;  (path->string (normalize-path (build-path project-path))))
