#lang setup/infotab

(define name "LeftParen")

(define required-core-version "4.1.3.9")

(define version "0.51")

(define release-notes
  '((p "In this version, all PLaneT \"requires\" reference an explict minor mode.  This should avoid issues with the updating of 3rd party PLaneT package.  See the previous release (0.5) for more info.")))

(define primary-file "leftparen.scm")

(define blurb
  '((p "LeftParen lets you make web apps quickly.")))

(define scribblings '(("scribblings/leftparen.scrbl" ())))

(define repositories '("4.x"))

(define categories '(devtools net))
