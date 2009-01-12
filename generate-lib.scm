#lang scheme/base

;;
;; code to generate basic files and folders needed for a project
;;

(require "util.scm"
         (only-in mzlib/file normalize-path))

(provide generate-project)

(define PLANET_MAJOR_VERISON 4)
(define PLANET_MINOR_VERSION 1)

(define (generate-project cmd-line-args-vec)
  (match cmd-line-args-vec
         ((vector project-path)
          (generate-project-from-path project-path))
         (else (e "You must provide exactly one argument to generate--a path to a new project directory."))))

(define (generate-project-from-path fresh-project-path)
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
             (generate-script-dir fresh-project-path)
             (generate-htdocs-files fresh-project-path))))

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
   '(serve my-app)
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

(define (generate-htdocs-files project-path)
  (ensure-existence-of-dir! project-path #:must-previously-exist #t)
  (generate-file-with-expressions
   (build-path project-path "htdocs/page-not-found.html")
   (make-raw "<html><body>Page not found.</body></html>")))

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
                 (display (format "Created directory ~A\n" dir-path))))))

;; it's an error if filename in path already exists
(define (with-output-to-file-in-dir path-to-file thunk
                                    #:must-previously-exist (must-exist #f))
  (receive (path filename is-dir) (split-path path-to-file)
    (ensure-existence-of-dir! path #:must-previously-exist must-exist)
    (with-output-to-file (build-path path filename) thunk #:mode 'text #:exists 'error)))

(define (expr-for-lp-require filename-rel-to-lib-root)
  `(planet ,filename-rel-to-lib-root ("vegashacker" "leftparen.plt"
                                      ,PLANET_MAJOR_VERISON (= ,PLANET_MINOR_VERSION))))
