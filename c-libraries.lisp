(in-package :shipshape)

(defvar *expected-libs-file* ".expected_libs")

(defun copy-all-c-libs (manifest)
  ;;
  ;; record expected libs
  (let* ((path (uiop:subpathname (uiop:ensure-directory-pathname
                                  (local-c-library-path manifest))
                                 *expected-libs-file*))
         (to-include (mapcar #'first (mapcar #'uiop:ensure-list
                                             (libs-to-include manifest))))
         (libs (mapcar (lambda (x)
                         (namestring (cffi:foreign-library-pathname x)))
                       (remove-if (lambda (x) (member x to-include))
                                  (cffi:list-foreign-libraries)
                                  :key #'cffi:foreign-library-name))))
    (ensure-directories-exist (uiop:pathname-directory-pathname path))
    (with-open-file (s path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (format s "~{~a~^~%~}" libs)))
  ;;
  ;; copy included libs
  (map nil (lambda (x)
             (destructuring-bind (name &optional directive)
                 (if (listp x) x (list x))
               (cond
                 ((null directive) (copy-c-lib name manifest))
                 ((eq directive :recur) (copy-lib-and-recur name manifest)))))
       (libs-to-include manifest)))

(defun lib-name-to-path (name)
  (let ((lib (find name (cffi:list-foreign-libraries)
                   :key #'cffi:foreign-library-name)))
    (cffi:foreign-library-pathname lib)))

(defun copy-c-lib (cffi-name manifest)
  (assert (not *shipped*))
  (let* ((src (lib-name-to-path cffi-name))
         (dst (pathname (format nil "~a~a" (local-c-library-path manifest)
                                (pathname-file-name src)))))
    (format t "~%--- COPYING ~S TO ~S" src dst)
    (ensure-directories-exist dst)
    (cl-fad:copy-file src dst)))

(defun copy-lib-and-recur (cffi-name manifest)
  (if (uiop:os-macosx-p)
      (let* ((src (lib-name-to-path cffi-name))
             (dst (uiop:ensure-directory-pathname (local-c-library-path manifest))))
        (format t "~%--- COPYING ~S TO ~S" src dst)
        (copy-and-fix-dylib
         src dst
         (uiop:subpathname* "@executable_path" (c-library-path manifest))))
      (progn
        (warn "Currently :recur is only supported on osx, falling back to copy")
        (copy-c-lib cffi-name manifest))))

(defun disconnect-all-c-libs ()
  (map 'nil #'cffi:close-foreign-library (cffi:list-foreign-libraries)))
