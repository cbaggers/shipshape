(in-package :shipshape)

(defun set-sail (system-name &optional (profile +default-profile+))
  (let ((manifest (find-manifest system-name profile)))
    ;; delete any existing build
    (ensure-no-directory (local-system-media-path manifest))
    (ensure-no-directory (local-c-library-path manifest))
    ;; Copy all files
    (copy-all-media manifest)
    (copy-all-c-libs manifest)
    ;;
    (transform-manifest-store-for-shipped)
    ;; and we are done!
    (setf *shipped* t)))

(defun dock ()
  (setf *shipped* nil)
  (transform-manifest-store-for-dev))
