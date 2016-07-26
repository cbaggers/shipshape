;;;; shipshape.asd

(asdf:defsystem #:shipshape
  :description "Library to help ship your lisp binary with c-libraries"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (#:trivial-dump-core #:cl-fad #:cffi)
  :components ((:file "package")
	       (:file "globals")
	       (:file "utils")
               (:file "dependencies")
               (:file "manifest")
	       (:file "path")
	       (:file "c-libraries")
	       (:file "media")
	       (:file "run")
	       (:file "ship")))
