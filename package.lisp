;;;; package.lisp

(defpackage #:shipshape
  (:use #:cl)
  (:export :def-shipping-manifest
	   :ship-it
	   :local-path))
