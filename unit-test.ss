#lang scheme

(require (planet "main.ss" ("schematics" "schemeunit.plt" 3 (= 3)))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" 3 (= 3)))
         "util.scm")
                 
(define basic-tests
  (test-suite
   "LeftParen basic test suite"
   ;;                              actual:        expected:
   (test-equal? "numeric equality" 1              1        )
   (test-equal? "md5 hashing" (md5-string "hello") "5d41402abc4b2a76b9719d911017c592")
   ))

(run-tests basic-tests)
