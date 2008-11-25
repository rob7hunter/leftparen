#lang scheme/base

;; a one-stop require that handles the "any" conflict with PLT contracts and SRFI 1.

(require scheme/contract)

(provide (combine-out (except-out (all-from-out scheme/contract) any)
                      (rename-out (any c:any))))

