(in-package :shipshape)

(defun copy-all-c-libs (manifest)
  (map nil (lambda (x) (copy-c-lib x manifest)) (cffi:list-foreign-libraries)))

(defun copy-c-lib (library manifest)
  (assert (not *shipped*))
  (let* ((src (cffi:foreign-library-pathname library))
         (dst (pathname (format nil "~a~a" (local-c-library-path manifest)
                                (pathname-file-name src)))))
    (ensure-directories-exist dst)
    (cl-fad:copy-file src dst)))
