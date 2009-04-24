#lang scheme

(require (planet "main.ss" ("schematics" "schemeunit.plt" 3 (= 3)))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" 3 (= 3)))
         "util.scm"
         "closures.scm")
                 
(define basic-tests
  (let ((clos-key 'dummy))
    (test-suite
     "LeftParen basic test suite"
     ;;                              actual:        expected:
     (test-equal? "numeric equality" 1              1        )
     (test-equal? "md5 hashing" (md5-string "hello") "5d41402abc4b2a76b9719d911017c592")
     (test-equal? "num closures in memory at start" (num-closures-in-memory) 0)
     
     (test-equal? "after a closure made"
                  (begin (set! clos-key (body-as-closure-key (req) "hi"))
                         (num-closures-in-memory))
                  1)
     (test-equal? "call a closure--check it's value"
                  (call-closure clos-key 'dummy-req)
                  "hi")

     (test-equal? "closures are usable only once"
                  (num-closures-in-memory)
                  0)
                         
     )))

(run-tests basic-tests)
