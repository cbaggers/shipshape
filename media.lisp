(in-package :shipshape)


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
  (map nil #'(lambda (x) (copy-all-media-for-single-manifest x manifest))
       (cons manifest (find-dependent-manifests (system manifest)
                                                :profile (shipping-profile manifest)
                                                :flat t))))


(defun copy-all-media-for-single-manifest (source-manifest target-manifest)
  (assert (not *shipped*))
  (with-slots (system copy-paths) source-manifest
    (let ((paths (validate-and-process-copy-paths system copy-paths)))
      (map nil (lambda (p) (copy-media p target-manifest)) paths))))


(defun copy-media (path manifest)
  (cond ((cl-fad:directory-exists-p path) (copy-dir path manifest))
        ((cl-fad:file-exists-p path) (copy-file path manifest))
        (t (error "Invalid path ~s~%This is a bug, please report it at:~%https://github.com/cbaggers/shipshape/issues"
                  path))))

(defun copy-file (path manifest)
  (with-slots (system system-media-path) manifest
    (let* ((src (local-path path system))
           (dst (merge-pathnames (pathname-file-name src)
                                 (local-media-path manifest))))
      (ensure-directories-exist dst)
      (cl-fad:copy-file src dst :overwrite t))))


(defun copy-dir (path manifest)
  (cl-fad:walk-directory path (lambda (x) (copy-file x manifest))))
