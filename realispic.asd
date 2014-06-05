;;;; realispic.asd

(asdf:defsystem #:realispic
    :serial t
    :depends-on (#:basicl
		 #:stefil
		 #:parenscript
                 #:hunchentoot
                 #:html-template)
    :components ((:file "package")
		 (:file "core")))

