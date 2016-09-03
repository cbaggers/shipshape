(in-package :shipshape)

;; Big thanks to Mike Ash for this post:
;; https://www.mikeash.com/pyblog/friday-qa-2009-11-06-linking-and-install-names.html
;; and to Shinmera for pointing me in the right direction with his qt-libs code

(defmacro with-chdir ((to) &body body)
  (let ((current (gensym "CURRENT"))
        (dir (gensym "TO")))
    `(let ((,current (uiop:getcwd))
           (,dir (uiop:pathname-directory-pathname ,to)))
       (assert (uiop:directory-exists-p ,dir))
       (unwind-protect (progn (uiop:chdir ,dir) ,@body)
         (uiop:chdir ,current)))))

(defun up-dir (pathname)
  (let ((pathname (uiop:pathname-directory-pathname pathname)))
    (uiop:make-pathname* :directory (butlast (pathname-directory pathname)))))

(defun otool-l (pathname)
  (with-chdir (pathname)
    (labels ((trim (x) (string-trim '(#\tab #\space) x))
             (process (s)
               (let ((p (search " (compat" s)))
                 (if p
                     (list (uiop:parse-unix-namestring (subseq s 0 p) :dot-dot :up)
                           (subseq s (1+ p)))
                     s))))
      (let* ((cmd (format NIL "otool -L ~s" (uiop:unix-namestring pathname)))
             (raw (uiop:run-program cmd :output :string))
             (lines (uiop:split-string raw :separator '(#\newline)))
             (clean (remove-if #'uiop:emptyp (mapcar #'trim (rest lines)))))
        (mapcar #'process clean)))))

(defun otool-get-rpath (pathname)
  (let* ((cmd (format nil "otool -l ~s" (uiop:unix-namestring pathname)))
         (load-str (uiop:run-program cmd :output :string))
         (split-pos (search "LC_RPATH" load-str)))
    (when split-pos
      (let* ((load-data (uiop:split-string (subseq load-str split-pos)
                                           :separator '(#\newline)))
             (raw-line (loop :for line :in load-data
                          :for trimmed := (string-trim '(#\tab #\space) line)
                          :when (alexandria:starts-with-subseq "path" trimmed)
                          :return (subseq trimmed #.(length "path "))
                          :when (alexandria:starts-with-subseq "Load" trimmed)
                          :return nil)))
        (let ((p (search " (" raw-line)))
                 (if p
                     (list (uiop:parse-unix-namestring (subseq raw-line 0 p))
                           (subseq raw-line (1+ p)))
                     raw-line))))))

(defun dylib-dependencies (pathname)
  ;; First two lines are the file itself again.
  (mapcar #'first (rest (otool-l pathname))))


(defun rpath-p (pathname)
  (when pathname
    (equal (second (pathname-directory pathname))
           "@rpath")))

(defun exepath-p (pathname)
  (when pathname
    (equal (second (pathname-directory pathname))
           "@executable_path")))

(defun loader-path-p (pathname)
  (when pathname
    (equal (second (pathname-directory pathname))
           "@loader_path")))

(defun absolute-child (x y)
  (when (and x y)
    (equal (search
            (pathname-directory (uiop:pathname-directory-pathname x))
            (pathname-directory (uiop:pathname-directory-pathname y))
            :test #'string=)
           0)))

(defun a-lib-filter (lib dependency)
  ;; will always be absolute
  (assert (and (uiop:absolute-pathname-p lib)
               (uiop:absolute-pathname-p dependency)))

  (or
   ;; dependency is in same or child folder of lib
   (absolute-child lib dependency)
   ;; dependency is in the default macports install folder
   (absolute-child #p"/opt/local" dependency)
   ;; dependency is in brew install folder
   (absolute-child (brew-prefix) dependency)))

(defun make-lib-filter (original-lib)
  (lambda (lib dependency)
    (assert (uiop:absolute-pathname-p dependency))
    (let ((r (or (absolute-child original-lib dependency)
                 (a-lib-filter lib dependency))))
      ;;(format t "~%filter ~a ~a" dependency r)
      r)))

(defun expand-loader-path (pathname loader-path)
  (assert (loader-path-p pathname))
  (let ((loader-path (uiop:pathname-directory-pathname
                      (uiop:ensure-pathname loader-path))))
    (uiop:make-pathname*
     :directory (append (pathname-directory loader-path)
                        (cddr (pathname-directory pathname)))
     :name (pathname-name pathname)
     :type (pathname-type pathname))))


(let ((cache nil))
  (defun brew-prefix ()
    "Returns brew's prefix path or nil"
    (or cache
        (multiple-value-bind (res _ err) (uiop:run-program "brew --prefix" :output :string :ignore-error-status t)
          (declare (ignore _))
          (if (= err 0)
              (setf cache
                    (uiop:ensure-directory-pathname
                     (uiop:parse-unix-namestring (string-trim '(#\newline) res))))
              nil)))))

(let ((seen nil))
  (defun calc-libs-rpath (lib)
    (assert (uiop:file-exists-p lib))
    (or (cdr (assoc lib seen))
        (let* ((rp (first (otool-get-rpath lib))))
          (when rp
            (let ((rp (uiop:ensure-directory-pathname
                        (if (loader-path-p rp)
                            (expand-loader-path rp lib)
                            rp))))
              (push (cons lib rp) seen)
              rp))))))

(defun rpath-to-absolute (pathname rpath)
  (assert (rpath-p pathname))
  (assert (uiop:absolute-pathname-p rpath))
  (uiop:make-pathname*
   :directory (append (pathname-directory rpath)
                      (cddr (pathname-directory pathname)))
   :name (pathname-name pathname)
   :type (pathname-type pathname)))

(defun find-rpath (incomplete-rpath rpath-stack)
  (assert (rpath-p incomplete-rpath))
  (loop :for potential :in rpath-stack
     :for complete := (rpath-to-absolute incomplete-rpath potential)
     :when (uiop:file-exists-p complete) :return complete))

(defun process-dep-path (pathname rpath-stack)
  (let* ((pathname (unless (exepath-p pathname)
                     (if (rpath-p pathname)
                         (or (find-rpath pathname rpath-stack)
                             pathname)
                         pathname))))
    (assert (or (null pathname) (uiop:absolute-pathname-p pathname)))
    pathname))

(defun initial-rpath-stack ()
  (let ((x (loop :for x :in cffi:*darwin-framework-directories*
              :append (apply (first x) (rest x)))))
    (append x (mapcar #'up-dir x))))

(defun walk-dylib-dependencies (source &key rpaths filter)
  (walk-dep-inner
   source
   (append (uiop:ensure-list rpaths) (initial-rpath-stack))
   (or filter (make-lib-filter source))))

(defun walk-dep-inner (source rpath-stack filter)
  (let ((source (uiop:ensure-pathname source :want-file t)))
    (assert (uiop:file-exists-p source))
    (assert (uiop:absolute-pathname-p source))
    (let* ((rpath-stack (let ((new-rpath (calc-libs-rpath source)))
                          (if new-rpath
                              (cons new-rpath rpath-stack)
                              rpath-stack)))
           (dependencies (mapcar (lambda (x) (process-dep-path x rpath-stack))
                                 (dylib-dependencies source))))
      (list
       source
       (mapcar (lambda (x) (walk-dep-inner x rpath-stack filter))
               (remove-if-not (lambda (x) (funcall filter source x))
                              (remove nil dependencies)))))))


(defun test (&optional (lib "/Library/Frameworks/SDL2_mixer.framework/Frameworks/smpeg2.framework/Versions/A/smpeg2"))
  (copy-and-fix-dylib
   lib
   (asdf:system-relative-pathname :shipshape "foo/")
   "@executable_path/foo"))

(defun test2 ()
  (walk-dylib-dependencies
   "/Library/Frameworks/SDL2_mixer.framework/SDL2_mixer"))

;;----------------------------------------------------------------------

(defun dylib-set-options (pathname &key name dependencies)
  (assert (evenp (length dependencies)) ()
          "Must supply a balanced number of DEPENDENCY and NEW pairs.")
  (with-chdir (pathname)
    (uiop:run-program
     (format nil "install_name_tool ~@[-id ~s ~] ~{-change ~s ~s ~} ~s"
             name dependencies (uiop:unix-namestring pathname))
     :output :string :error-output t)))

(defun %fix-dylib (f swap-rpath-for)
  (labels ((new-path (o)
             (uiop:make-pathname*
              :directory (pathname-directory swap-rpath-for)
              :name (pathname-name o)
              :type (let ((pt (pathname-type o)))
                      (unless (eq :unspecific pt) pt))))
           (new-name (x)
             (cond
               ((rpath-p x) (uiop:unix-namestring (new-path x)))
               ((exepath-p x) (uiop:unix-namestring (new-path x)))
               (t (uiop:unix-namestring x))))
           (pair (x)
             (list (uiop:unix-namestring x)
                   (new-name x)))
           (pairs (x)
             (mapcar #'pair (dylib-dependencies x))))
    (let ((pairs (pairs f)))
      ;;(format t "~%fixing ~s~%with ~s" f pairs)
      (values pairs
              (dylib-set-options
               f :name (new-name f) :dependencies (reduce #'append pairs))))))

(defun copy-and-fix-dylib (lib to swap-rpath-for)
  (let* ((to (uiop:ensure-directory-pathname to))
         (swap-rpath-for (uiop:ensure-directory-pathname swap-rpath-for))
         (lib-and-dependencies
          (remove-duplicates
           (cons (uiop:parse-unix-namestring lib)
                 (alexandria:flatten (walk-dylib-dependencies lib)))
           :test #'pathname-match-p)))
    (labels ((dest-file-name (f)
               (uiop:ensure-pathname
                (uiop:subpathname to (pathname-name f)
                                  :type (if (eq :unspecific (pathname-type f))
                                            nil
                                            (pathname-type f)))
                :want-file t))
             (copy-and-fix (l)
               (let ((d (dest-file-name l)))
                 (uiop:copy-file l d)
                 (%fix-dylib d swap-rpath-for))))
      (ensure-directories-exist to)
      (mapcar #'copy-and-fix lib-and-dependencies))))
