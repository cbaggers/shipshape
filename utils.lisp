(in-package :shipshape)

(defun pathname-file-name (pathname)
  (let ((type (pathname-type pathname))
	(name (pathname-name pathname)))
    (if type
	(format nil "~a.~a" name type)
	name)))
