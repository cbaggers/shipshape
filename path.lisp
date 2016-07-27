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

(defun local-c-library-path (manifest)
  (assert (not *shipped*))
  (local-path (c-library-path manifest)
	      (system manifest)))

(defun local-system-media-path (manifest)
  (assert (not *shipped*))
  (local-path (system-media-path manifest)
	      (system manifest)))

;; this is a hack, tidy the
(defun shipped-c-library-path (manifest)
  (cl-fad:pathname-directory-pathname
   (merge-pathnames (c-library-path manifest)
		    (cl-fad:pathname-directory-pathname
		     (first sb-ext:*posix-argv*)))))

(defun ensure-no-directory (pathname)
  (when (cl-fad:directory-exists-p pathname)
    (cl-fad:delete-directory-and-files pathname)))
