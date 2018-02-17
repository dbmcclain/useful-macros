
(asdf:defsystem "useful-macros"
  :description "useful-macros: a collection of widely useful macros and functions"
  :version     "1.0"
  :author      "D.McClain <dbm@spectrodynamics.com>"
  :license     "Copyright (c) 2008 by SpectroDynamics, LLC. All rights reserved."
  :components  ((:file "packages")
                #+LISPWORKS (:file "package-aliases")
                (:file "reader-macros")
                #+LISPWORKS (:file "ctypes")
                (:file "useful-macros")
                (:file "remembered-filenames")
                ;; (:file "useful-macros-old")
                ;; (:file "match-macro")
                (:file "match-macro-ex")
                (:file "match-macro-ex-opt")
                ;; (:file "match-macro-ex3")
                ;; (:file "monitor-macros")
                (:file "memoize")
                (:file "cache")
                #+:WIN32 (:file "exec")
                ;; (:file "lazy") ;; supplanted by a better, simpler, version
                (:file "engfmt")
                (:file "usec")
               	(:file "uuid")
                #+LISPWORKS (:file "computed-metaclass")
                #+(AND :LISPWORKS :MACOSX) (:file "OSX-UUID-Generate")
                #+(AND :ALLEGRO :MACOSX)   (:file "OSX-UUID-Generate-Allegro")
                )
  :serial       t
  :depends-on   (#| "compiled-ml-matcher" |#
                 "let-over-lambda"
                 "bordeaux-threads"
                 "cl-ppcre"
                 "ironclad"))

