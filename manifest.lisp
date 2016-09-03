(in-package :shipshape)


(defvar *manifests* (make-hash-table :test #'equal))

(defun key (system profile)
  (if *shipped*
      (asdf:coerce-name system)
      (progn
        (assert (keywordp profile))
        (cons (asdf:coerce-name system) profile))))


(defclass shipping-manifest ()
  ((system :initform nil :initarg :system
           :accessor system)
   (shipping-profile :initform +default-profile+ :initarg :profile
                     :accessor shipping-profile)
   (main-function-name :initform nil :initarg :main-function-name
                       :accessor main-function-name)
   (build-path :initform nil :initarg :build-path
               :accessor build-path)
   (binary-name :initform nil :initarg :binary-name
                :accessor binary-name)
   (c-library-path :initform "" :initarg :c-library-path
                   :accessor c-library-path)
   (system-media-path :initform "media/" :initarg :system-media-path
                      :accessor system-media-path)
   (copy-paths :initform nil :initarg :copy-paths
               :accessor copy-paths)
   (compression :initform nil :initarg :compression
                :accessor compression)
   (libs-to-include :initform nil :initarg :libs-to-include
                    :accessor libs-to-include)))


(defun find-manifest (system &optional (profile +default-profile+)
                               error-if-missingp)
  (or (gethash (key system profile) *manifests*)
      (when error-if-missingp
        (error "Manifest could not be found for system ~s with profile ~s"
               system profile))))


(defun add-manifest (manifest)
  (with-slots (system shipping-profile) manifest
    (let ((key (key system shipping-profile)))
      (when (gethash key *manifests*)
        (warn "A manifest for system ~s with build profile ~s already existed.
 Replacing" system (shipping-profile manifest)))
      (setf (gethash key *manifests*) manifest))))



(defun ensure-dir-name (path)
  (if (char= (elt path (1- (length path))) #\/)
      path
      (format nil "~a/" path)))


(defmethod initialize-instance :after ((manifest shipping-manifest) &key)
  (with-slots (system shipping-profile system-media-path c-library-path copy-paths
                      binary-name build-path)
      manifest
    (setf system (asdf:coerce-name system)
          system-media-path (ensure-dir-name system-media-path)
          c-library-path (ensure-dir-name c-library-path)
          build-path (ensure-dir-name build-path)
          binary-name (pathname-file-name
                       (etypecase binary-name
                         (symbol (string-downcase binary-name))
                         ((or pathname string) binary-name))))
    ;; we only validate the path can be a valid pathname here, we leave
    ;; checking if the file/dir exists until later as technically the target
    ;; could be generated at compile-time. We only need to check they exist
    ;; when setting sail
    (let* ((paths (mapcar #'cons (mapcar #'pathname copy-paths) copy-paths))
           (problem-paths (mapcar #'cdr (remove-if-not #'car paths))))
      (unless (every #'identity paths)
        (error "Could not make manifest ~s for profile ~s as the following
could not make into valid pathnames:~{~%~s~}"
               system shipping-profile problem-paths))))
  manifest)


(defun find-dependent-manifests (system &key (profile +default-profile+) flat)
  (walk-dependencies system (lambda (x) (find-manifest x profile)) :flat flat))


(defun transform-manifest-store-for-shipped (profile)
  (let ((new-table (make-hash-table :test #'equal)))
    (maphash (lambda (k v)
               (when (eq (cdr k) profile)
                 (setf (gethash (car k) new-table) v)))
             *manifests*)
    (setf *manifests* new-table )))


(defun transform-manifest-store-for-dev ()
  (let ((new-table (make-hash-table :test #'equal)))
    (maphash (lambda (k v)
               (setf (gethash (key k (shipping-profile v)) new-table) v))
             *manifests*)
    (setf *manifests* new-table )))


;;----------------------------------------------------------------------
;; Macro

(defmacro def-shipping-manifest (system main-function-name &body args)
  (let ((system (asdf:coerce-name system)))
    (assert (symbolp main-function-name))
    (destructuring-bind (profile build-path binary-name c-library-path
                                 system-media-path paths compression
                                 libs-to-include)
        (check-macro-args (parse-macro-args system args))
      `(add-manifest
        (make-instance 'shipping-manifest
                       :system ',system
                       :profile ',profile
                       :build-path ,build-path
                       :binary-name ,binary-name
                       :main-function-name ',main-function-name
                       :c-library-path ,c-library-path
                       :system-media-path ,system-media-path
                       :copy-paths ',paths
                       :compression ,compression
                       :libs-to-include ',libs-to-include)))))

(defun check-macro-args (args)
  (destructuring-bind (profile build-path binary-name c-library-path
                               system-media-path paths compression
                               libs-to-include) args
    (assert (symbolp profile))
    (assert (stringp binary-name))
    (assert (or (stringp c-library-path)
                (pathnamep c-library-path)))
    (assert (or (stringp system-media-path)
                (pathnamep system-media-path)))
    (assert (or (stringp build-path)
                (pathnamep build-path)))
    (assert (every #'string paths))
    (assert (and (numberp compression)
                 (>= compression -1)
                 (<= compression 9)))
    (assert (and (listp libs-to-include)
                 (every (lambda (x)
                          (or (stringp x)
                              (pathnamep x)
                              (symbolp x)
                              (and (listp x)
                                   (= (length x) 2)
                                   (symbolp (first x))
                                   (keywordp (second x)))))
                        libs-to-include)))
    args))

(defun default-binary-name (system)
  #+windows
  (format nil "~a.exe" system)
  #-windows
  system)

(defun default-build-path (system)
  (format nil "build-~a" system))

(defun parse-macro-args (system args)
  (let* ((profile :ship)
         (build-path (default-build-path system))
         (binary-name (default-binary-name system))
         (c-library-path "c-deps")
         (system-media-path "sys-media")
         (libs-to-include nil)
         (paths nil)
         (compression -1))
    (labels ((eat-something (e)
               (etypecase (first e)
                 ((or pathname string) (eat-paths e))
                 (keyword (eat-pair e))))
             (eat-paths (e)
               (setf paths e))
             (eat-pair (e)
               (destructuring-bind (k v . rest) e
                 (set-pair k v)
                 (eat-something rest)))
             (set-pair (k v)
               (ecase k
                 (:build-path (setf build-path v))
                 (:profile (setf profile v))
                 (:binary-name (setf binary-name v))
                 (:c-library-path (setf c-library-path v))
                 (:system-media-path (setf system-media-path v))
                 (:compression (setf compression v))
                 (:libs-to-include (setf libs-to-include v)))))
      (eat-something args)
      (list profile
            build-path
            binary-name
            c-library-path
            system-media-path
            paths
            compression
            libs-to-include))))
