;;;; realispic.asd

(asdf:defsystem #:realispic
    :serial t
    :depends-on (#:basicl
		 #:stefil
		 #:parenscript)
    :components ((:file "package")
		 (:file "core")))
