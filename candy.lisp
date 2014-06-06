;;;; candy.lisp
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.realispic)

(defpsmacro local-node (name)
  `(chain this refs ,name (get-d-o-m-node)))



