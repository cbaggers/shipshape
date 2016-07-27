(in-package :shipshape)

;; before shipped path is local to system
;; after shipped (and if has manifest) is local to media/system-name/

(defun local-path (path system)
  ;; local-media-path is inlines as then the compiler will optimize away
  ;; the redudent conditionals
  (declare (inline local-media-path))
  (let ((manifest (when *shipped* (find-manifest system))))
    (if manifest
        (local-media-path manifest)
        (asdf:system-relative-pathname system path))))


(defun local-media-path (manifest &optional (path ""))
  (with-slots (system system-media-path) manifest
    (if *shipped*
        (reduce #'(lambda (x y) (merge-pathnames y x))
                (list system-media-path
                      (format nil "~a/" system)
                      path)
                :initial-value (directory-namestring
                                (first sb-ext:*posix-argv*)))
        (asdf:system-relative-pathname system path))))


(defun local-c-library-path (manifest &optional (path ""))
  (with-slots (system c-library-path) manifest
    (reduce #'(lambda (x y) (merge-pathnames y x))
            (list c-library-path
                  path)
            :initial-value
            (if *shipped*
                (directory-namestring (first sb-ext:*posix-argv*))
                (asdf:system-relative-pathname system "")))))


(defun ensure-no-directory (pathname)
  (when (cl-fad:directory-exists-p pathname)
    (cl-fad:delete-directory-and-files pathname)))
