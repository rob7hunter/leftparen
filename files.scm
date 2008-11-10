#lang scheme/base

(require (file "util.scm")
         (file "web-support.scm")
         "settings.scm")

(provide save-uploaded-file-and-return-filename!)

(declare-setting *PATH_TO_UPLOADED_FILES* (build-path (current-directory) "uploaded-files"))

(define (save-uploaded-file-and-return-filename! file-data)
  (let* ((filename (fresh-filename-id (binding/string:file-filename file-data)))
         (raw-file-bytes (binding/string:file-content file-data))
         (fport (open-output-file (build-path (setting *PATH_TO_UPLOADED_FILES*) filename)
                                  #:exists 'error)))
    (write-bytes raw-file-bytes fport)
    (close-output-port fport)
    filename))

(define (fresh-filename-id filename #:id-length (id-length 5))
  (let ((try (string-append (random-key-string id-length) "-" filename)))
    (if (file-exists? (build-path (setting *PATH_TO_UPLOADED_FILES*) try))
        (fresh-filename-id filename #:id-length id-length)
        try)))
