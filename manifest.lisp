(in-package :shipshape)


(defvar *manifests* (make-hash-table :test #'equal))


(defun key (system profile)
  (if *shipped*
      system
      (progn
	(assert (keywordp profile))
	(cons (asdf:coerce-name system) profile))))


(defclass shipping-manifest ()
  ((system :initform nil :initarg :system
	   :accessor system)
   (profile :initform +default-profile+ :initarg :profile
            :accessor profile)
   (c-library-path :initform "" :initarg :c-library-path
                   :accessor c-library-path)
   (system-media-path :initform "media/" :initarg :system-media-path
                      :accessor system-media-path)
   (copy-paths :initform nil :initarg :copy-paths
               :accessor copy-paths)))

(defun find-manifest (system &optional (profile +default-profile+)
                               error-if-missingp)
  (or (gethash (key system profile) *manifests*)
      (when error-if-missingp
        (error "Manifest could not be found for system ~s with profile ~s"
               system profile))))


(defun add-manifest (manifest)
  (with-slots (system profile) manifest
    (let ((key (key system profile)))
      (when (gethash key *manifests*)
        (warn "A manifest for system ~s with build profile ~s already existed.
 Replacing" system (profile manifest)))
      (setf (gethash key *manifests*) manifest))))


(defmacro def-shipping-manifest (system
                                 &key (profile :+default-profile+)
                                   c-library-path copy-paths
                                   (system-media-path "media/"))
  `(add-manifest
    (make-instance 'shipping-manifest
                   :system ',system
                   :profile ',profile
                   :c-library-path ',c-library-path
                   :system-media-path ',system-media-path
                   :copy-paths ',copy-paths)))

(defun ensure-dir-name (path)
  (if (char= (elt path (1- (length path))) #\/)
      path
      (format nil "~a/" path)))

(defmethod initialize-instance :after ((manifest shipping-manifest) &key)
  (print "IMPLEMENT ME! #'(initialize-instance shipping-manifest)")
  (with-slots (system system-media-path c-library-path) manifest
    (setf system (asdf:coerce-name system)
          system-media-path (ensure-dir-name system-media-path)
          c-library-path (ensure-dir-name c-library-path)))
  manifest)


(defun find-dependent-manifests (system &key (profile +default-profile+) flat)
  (walk-dependencies system (lambda (x) (find-manifest x profile)) :flat flat))


(defun transform-manifest-store-for-shipped ()
  (let ((new-table (make-hash-table :test #'equal)))
    (maphash (lambda (k v)
	       (setf (gethash (car k) new-table) v))
	     *manifests*)
    (setf *manifests* new-table )))

(defun transform-manifest-store-for-dev ()
  (let ((new-table (make-hash-table :test #'equal)))
    (maphash (lambda (k v)
	       (setf (gethash (key k (profile v)) new-table) v))
	     *manifests*)
    (setf *manifests* new-table )))
