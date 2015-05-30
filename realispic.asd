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
                 (:file "css")
		 (:file "core")
                 (:file "compiler")
                 (:file "def-widget")
		 (:file "def-app")
                 (:file "candy")
                 (:file "rpc")
                 (:file "unit-test/realispic-test")))

