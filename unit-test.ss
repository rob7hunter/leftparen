(require (planet "main.ss" ("schematics" "schemeunit.plt" 3 (= 3)))
         (planet "text-ui.ss" ("schematics" "schemeunit.plt" 3 (= 3))))
                 
(define basic-tests
  (test-suite
   "LeftParen basic test suite"
   ;;                              actual:  expected:
   (test-equal? "numeric equality" 1        2        )
   ))

(run-tests basic-tests)
