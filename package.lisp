;;;; package.lisp

(defpackage #:breakds.realispic
  (:nicknames #:realispic)
  (:use #:cl
	#:parenscript)
  (:import-from #:swiss-knife
                #:mkstr
                #:with-gensyms
                #:symb
		#:mk-keyword
		#:aif
		#:awhen
		#:it
		#:group)
  (:export *realispic-symbol-table*
           #:enable-jsx-reader
           #:disable-jsx-reader
           #:def-widget
           #:def-widget-1
           #:def-global-code
           #:def-realispic-app
           ;; RPC
           #:def-rpc
           #:with-rpc
	   #:rpc-result
	   #:current-session
           ;; Candies
           #:local-node
	   #:local-state
           #:llambda
           #:json))

