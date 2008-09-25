#lang scheme/base

;;XXX change name to lib-sig.scm

(require mzlib/defmacro
         scheme/unit
         (for-syntax scheme/base
                     "lib-bindings.scm"))

(provide lib^)

(define-macro (define-signature-lib^)
  `(define-signature lib^ ,lib-bindings))

(define-signature-lib^)
