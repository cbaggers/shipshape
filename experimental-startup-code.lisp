;;;; experimental-proj

(in-package #:experimental-proj)

(defun experimental-proj ()
  (print "begin test")
  (let ((local (directory-namestring (first sb-ext:*posix-argv*))))
    (push (merge-pathnames "c-lib/" local) cffi:*foreign-library-directories*)
    (cffi:load-foreign-library 'cl-soil::soil)
    (print (cl-soil:load-image (merge-pathnames "alot.png" local))))
  (print "and we are done"))
