;;;; shipshape.asd

(asdf:defsystem #:shipshape
  :description "Library to help ship your lisp binary with c-libraries"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (#:cl-fad)
  :components ((:file "package")
	       (:file "globals")
               (:file "dependencies")
               (:file "manifest")
	       (:file "path")
	       (:file "ship")))
