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

;;; llambda (local lambda) is a parenscript macro that bind the this
;;; to the scope instead of the caller.
(defpsmacro llambda (args &body body)
  `(chain (lambda ,args ,@body)
          (bind this)))

(defpsmacro map (fun list &key this)
  `((@ ,list map) ,fun ,this))

(defpsmacro rand-int (end)
  `(funcall (@ *math floor)
            (* (funcall (@ *math random)) ,end)))

(defpsmacro trace (content)
  `((@ console log) ,content))

(defmacro json (&rest items)
  `(list :obj ,@(mapcar #`(cons ,(car x1) ,(cadr x1))
                        (group items 2))))


