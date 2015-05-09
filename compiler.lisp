;;;; compiler.lisp
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.realispic)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun code-walker (form 
