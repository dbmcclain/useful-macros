
(in-package "CL-USER")

(defpackage #:useful-macros
  (:use #:common-lisp)
  (:nicknames #:um)
  (:export
   #:with
   #:letp
   #:wholepart
   #:fracpart
   #:copy-struct
   #:make-struct-copy
   #:slot-names
   
   #:read-extended-number-syntax
   #:hz-to-nn
   #:nn-to-hz
   
   #:symbol-gensym
   #:with-gensyms
   #:allf
   #:nilf
   #:tf
   #:conc1f
   #:addf ;; same as incf
   #:subf
   #:mulf
   #:divf
   #:deletef
   #:deletef-if
   #:removef
   #:removef-if
   #:aconsf
   #:self-init
   #:ensure-assoc
   
   #:while
   #:foreach
   #:until
   #:if-let
   #:when-let
   #:if-let*
   #:when-let*
   
   #:append1
   #:conc1
   #:single
   #:last1
   #:mklist
   #:longer
   #:filter
   #:group
   #:flatten
   #:prune
   #:find2
   #:before
   #:after
   #:duplicate
   #:split-if
   #:most
   #:best
   #:mostn
   #:map0-n
   #:map1-n
   #:mapa-b
   #:map->
   #:mappend
   #:mapcars
   #:rmapcar
   #:readlist
   #:prompt
   #:break-loop
   #:mkstr
   #:raw-mkstr
   #:symb
   #:reread
   #:explode
   #:correct-for-symbol-character-case
   #:intern-symbol
   
   #:constituent
   #:tokens
   #:tokens-if
   #:tokens-if-not
   #:split-string
   #:paste-strings
   ;; #:getenv
   ;; #:winexec
   #:pickfile
   #:get-time-string
   #:map-from-to
   #:map-from-below
   #:map-from-for
   #:where
   #:where-not
   #:subselect
   #:indgen
   #:collect-where
   #:collect-where-not
   #:collect-if
   #:collect-if-not
   #:keep-if
   #:keep-if-not
   #:with-cstring
   #:with-cstrings
   #:in-cstring
   #:out-cstring
   #:uchar
   #:def-enum
   #:ez-define-foreign-function

   #:compose
   #:curry
   #:rcurry
   #:combine

   #:expanded-compose
   #:expanded-curry
   #:expanded-rcurry
   #:expanded-combine

   #:curried-lambda

   #:eqlcond
   #:foldl
   #:foldr
   #:coerce-fli-arg
   #:def-safe-fli-function

   #:def-once-only
   #:once-function

   #:with-tail-pure-code
   #:true
   #:false
   #:do-nothing
   
   ;; lazy eval and once-functions
   #:lazy
   #:force
   #:deferred
   ;; #:lazy-once
   ;; #:lazy-once-thereafter

   #:make-collector
   #:make-mpsafe-collector
   #:make-monitored-collector
   #:make-mpsafe-monitored-collector
   #:with-locked-instance
   #:changed-p
   #:collector-append-item
   #:collector-push-item
   #:collector-pop
   #:collector-contents
   #:collector-ncontents
   #:collector-empty-p
   #:collector-discard-contents
   #:collector-stuff-contents
   
   #:make-range
   #:range

   #:drop
   #:take
   #:split
   #:zip
   #:interleave

   #:make-once-thereafter
   #:get-value

   #:largest-abs-value
   #:make-list-reducer

   #:post-incf
   #:post-decf

   #:with-slot-values

   #:fn
   #:if*
   #:alambda
   #:aif
   #:it
   #:aif*
   #:awhen
   #:alet
   #:alet-fsm
   #:arun-fsm
   #:ichain-before
   #:ichain-after
   #:ichain-intercept
   #:alet-hotpatch
   #:let-hotpatch
   #:let-binding-transform
   #:sublet
   #:sublet*
   #:pandoriclet
   #:pandoriclet-get
   #:pandoriclet-set
   #:get-pandoric
   #:with-pandoric
   #:pandoric-hotpatch
   #:pandoric-recode
   #:plambda
   #:defpan
   #:pandoric-eval
   #:this
   #:self
   
   #:make-rubber-vector
   #:firsts-of
   #:slice
   #:left-part
   #:right-part

   #:*match-case-sensitive-p*
   #:match
   #:match2
   #:rt-match
   #:match-fail
   #:match-failure
   #:encode-match-body
   #:encode-match-bodies
   #:eql-tree
   
   #:move
   #:ceiling-pwr2
   #:ceiling-log2

   #:floor-pwr2
   #:floor-log2

   #:align-pwr2

   #:format-error
   #:separate-declares-and-documentation
   #:define-monitor
   #:with-monitor
   #:let-monitor
   #:lock-mixin
   #:let-locking

   #:with-slot-accessors
   #:bind*
   #:define-bind*-handler
   #:perform

   #:binsearch

   #:computed-metalevel-class

   #:nif
   #:g!-symbol-p
   #:defmacro/g!
   #:nlet
   #:nlet-tail
   #:o!-symbol-p
   #:o!-symbol-to-g!-symbol
   #:defmacro!
   #:dlambda
   #:dcase

   #:segment-reader

   #:make-state-machine
   #:run-state-machine
   #:push-state
   #:pop-state

   #:make-coll
   #:collect-decls
   #:dis

   #:fast-progn
   #:safe-progn

   #:pointer-&
   #:pointer-*
   #:with-fast-stack

   #:make-tlist
   #:tlist-left
   #:tlist-right
   #:tlist-empty-p
   #:tlist-add-left
   #:tlist-add-right
   #:tlist-rem-left
   #:tlist-update
   #:with-conses-counted
   #:with-cons-pool
   #:cons-pool-cons
   #:cons-pool-free
   #:make-cons-pool-stack
   #:make-shared-cons-pool-stack
   #:with-dynamic-cons-pools
   #:fill-cons-pool

   #:hhmmss.ss
   #:hms
   #:ddmmyyyy
   #:dmy
   #:yyyymmdd
   #:ymd


   #:memo
   #:memoize
   #:clear-memoize
   #:un-memoize
   #:defun-memo

   #:cache
   #:cacheize
   #:un-cacheize
   
   #:2-way-cache
   #:check-cache
   #:update-cache
   #:clear-cache

   #:2-way-n-level-cache
   #:check-cache
   #:update-cache
   #:clear-cache
   #:clear-cache-row

   #:set-$-dispatch-reader
   #:get-$-dispatch-reader
   #:set-/-dispatch-reader
   #:get-/-dispatch-reader
   #:read-chars-till-delim

   #:alias
   #:unalias
   #:aliases ;; use (setf aliases) to set new aliases
   #:defaliasfn
   #:defcapture

   #:separate-decls-and-body

   #:with-remembered-filename
   #:remember-filename
   #:remembered-filename
   #:filename-timestamp-string
   #:add-timestamp-to-filename

   #:defwrapper
   ))

(defpackage #:lazy
  (:use #:common-lisp)
  (:export
   #:make
   #:force))

(defpackage #:mlmatch
  (:use #:common-lisp)
  (:nicknames #:ml)
  (:export
   #:match
   ;; #:match-using-style
   ;; #:$default-matching-style
   ;; #:$case-sensitive-matching-style
   ;; #:match-fail
   #:match-failure
   ;; #:pattern-variable-p
   ;; #:wild-pattern-p
   ;; #:meta-match
   #:collect
   ))

(defpackage #:engfmt
  (:use #:common-lisp)
  (:export
   #:engineering-format
   #:scientific-format
   #:lower-case-e-exponent-printer
   #:always-signed-exponent-printer
   #:paren-style-exponent-printer
   #:mathematica-style-exponent-printer
   ))

