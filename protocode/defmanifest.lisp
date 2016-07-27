
;; this feel a little egh
(def-shipping-manifest test
    :profile :ship
    :binary-name "jam.bin"
    :main-function-name 'main
    :c-library-path "c-deps"
    :system-media-path "media"
    "test"
    "thamp"
    "some-dir/")


;; huh..thats much nicer :D
(def-shipping-manifest test :ship
  :binary-name "jam.bin"
  :main-function-name 'main
  :c-library-path "c-deps"
  :system-media-path "media"
  "test"
  "thamp")

;; but it's a bit of a shame as I wanted profile to be optional
;; we could make another one take it's place..but which?
;;
;; :main-function-name - nah as this helps dis...no wait!

(def-shipping-manifest test main-func
  :profile :ship
  :binary-name "jam.bin"
  :c-library-path "c-deps"
  :system-media-path "media"
  "test"
  "thamp")

;; YEAH! because then setting it to nil is an explicit way of
;; saying 'this is a library'. Cool :)


;; The minimal manifest. looks great!
(def-shipping-manifest vacuum run-vaccum
  "media/")


;; gotta process the args

(defun eat (system main-func args)
  (let ((profile :ship)
        (binary-name "jam.bin")
        (c-library-path "c-deps")
        (system-media-path "media")
        (paths nil))
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
                 (:profile (setf profile v))
                 (:binary-name (setf binary-name v))
                 (:c-library-path (setf c-library-path v))
                 (:system-media-path (setf system-media-path v)))))
      (eat-something args)
      (list system
            main-func
            binary-name
            c-library-path
            system-media-path
            paths))))
