;;;; candy.lisp
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.realispic)

;;; local-node is a parenscript macro to access the node with the
;;; reference name.
(defpsmacro local-node (name)
  `(chain this refs ,name (get-d-o-m-node)))

;;; local-state is a parenscript macro to access the local state with
;;; the specified name.
(defpsmacro local-state (name)
  `(@ this state ,name))



