(in-package :shipshape)

(defun set-sail ()
  (setf *shipped* t)
  (transform-manifest-store-for-shipped))

(defun dock ()
  (setf *shipped* nil)
  (transform-manifest-store-for-dev))
