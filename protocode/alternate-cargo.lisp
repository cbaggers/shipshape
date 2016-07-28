;; this will live in shipshape

(defmacro def-alternate-cargo (name &body alternate-path)
  )

(def-alternate-cargo "SDL"
  #+windows "lib/libSDL.dll"
  #+darwin "lib/libSDL.so"
  #+(and linux x86) "lib/libSDL.so"
  #+(and linux x86-64) "lib/libSDL64.so")

;; - finds and validates the library is in cffi
;; - maybe add the search path? yeah :)
;; - record in table so when copying for release we use the alternate-cargo
;;   instead
;; - if string is missing then silently ignore it...hmm this is ugly though
;;   we want to be able to say either 'use & copy default' or 'dont copy' and
;;   search with cffi
