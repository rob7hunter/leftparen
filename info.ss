#lang setup/infotab

(define name "LeftParen")

(define required-core-version "4.1.3.9")

(define version "0.5")

(define release-notes
  '((p "This version now uses the built-in serve/servlet functionality, and is no longer dependent on the deprecated Instaservlet package.  Other changes include")
    (ul (li "Requires PLT Scheme 4.1.3.9 which is an unreleased version (see the docs for info on getting this version).")
        (li "Pages in app.scm now are triggered " (em "before") " static files in htdocs.")
        (li "Generated project files are now simplified.  This means that there are minor upgrade steps needed to get your older LeftParen projects running on the new system.  I suggest creating a quick hello-world LeftParen (see the tutorial) and looking at the generated files to see how to adjust your existing project.  Feel free to email support at leftparen.com with questions."))))

(define primary-file "leftparen.scm")

(define blurb
  '((p "LeftParen lets you make web apps quickly.")))

(define scribblings '(("scribblings/leftparen.scrbl" ())))

(define repositories '("4.x"))

(define categories '(devtools net))
