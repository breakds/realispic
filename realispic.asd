;;;; realispic.asd

(asdf:defsystem #:realispic
    :serial t
    :depends-on (#:basicl
		 #:stefil
		 #:parenscript
                 #:hunchentoot
                 #:html-template
                 #:jsown)
    :components ((:file "package")
		 (:file "core")
                 (:file "def-widget")
                 (:file "candy")
                 (:file "rpc")))

