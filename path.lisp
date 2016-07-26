(in-package :shipshape)

;; before shipped path is local to system
;; after shipped (and if has manifest) is local to media/system-name/

(defun local-path (path system)
  (let ((manifest (when *shipped* (find-manifest system))))
    (if manifest
	(with-slots (system system-media-path) manifest
	  (reduce #'(lambda (x y) (merge-pathnames y x))
		   (list system-media-path
			 (format nil "~a/" system)
			 path)
		   :initial-value (directory-namestring
				   (first sb-ext:*posix-argv*))))
	(asdf:system-relative-pathname system path))))
