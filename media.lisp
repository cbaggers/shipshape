(in-package :shipshape)

;; - can be any type accepted by #'pathname

;; the system does the following
;; - get the absolute path relative to the system
;; - check if #'cl-fad:directory-exists-p
;;   - if yes then copy all containing files recursively
;;   - if no then check #'cl-fad:file-exists-p
;;      - if yes then copy file
;;      - if no then throw error

(defun validate-and-process-copy-paths (system paths)
  ;; We do this together so we can aggregate the error message
  ;; this makes it less tedious to fix
  (labels ((path (p) (local-path p system)))
    (let* ((absolute-paths (mapcar #'path paths))
	   (exist (mapcar (lambda (p)
			    (or (cl-fad:file-exists-p p)
				(cl-fad:directory-exists-p p)))
			  absolute-paths))
	   (problem (remove nil (mapcar (lambda (e a) (unless e a))
					exist absolute-paths))))
      (if problem
	  (error "Could not proceed with build as the following files/directories
don't exist:~{~%~s~}" problem)
	  absolute-paths))))


(defun copy-all-media (manifest)
  (assert (not *shipped*))
  (with-slots (system copy-paths) manifest
    (let ((paths (validate-and-process-copy-paths system copy-paths)))
      (map nil (lambda (p) (copy-media p manifest)) paths))))

(defun copy-media (path manifest)
  (cond ((cl-fad:directory-exists-p path) (copy-dir path manifest))
	((cl-fad:file-exists-p path) (copy-file path manifest))
	(t (error "Invalid path ~s~%This is a bug, please report it at:~%https://github.com/cbaggers/shipshape/issues"
		  path))))

(defun copy-file (path manifest)
  (with-slots (system system-media-path) manifest
    (let* ((src (local-path path system))
	   (dst (reduce #'(lambda (x y) (merge-pathnames y x))
			(list (format nil "~a/" system)
			      (pathname-file-name src))
			:initial-value (local-system-media-path manifest))))
      (ensure-directories-exist dst)
      (cl-fad:copy-file src dst))))

(defun copy-dir (path manifest)
  (cl-fad:walk-directory path (lambda (x) (copy-file x manifest))))
