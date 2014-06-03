;;;; core.lisp
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.realispic)


(declaim (inline regular-symbol))
(defun regular-symbol (keyword-symbol)
  (symb (mkstr keyword-symbol)))

(defvar *dom-tags* '(:html :body :head :div
                     :form :ul :li :input
                     :button :a))

(defun jsx-rewrite (jsx-expr)
  (labels ((transform-attribute (attribute)
             (case (car attribute)
               (t attribute))))
    (if (atom jsx-expr) jsx-expr
        `(,(cond ((member (car jsx-expr) *dom-tags*)
                  `(@ *react 
                      *dom* 
                      ,(regular-symbol (car jsx-expr))))
                 (t (car jsx-expr)))
           (create ,@(mapcan #'transform-attribute 
                             (second jsx-expr)))
           ,@(mapcar #'jsx-rewrite (cddr jsx-expr))))))

(defun jsx-reader (stream sub-char-a sub-char-b)
  (declare (ignorable sub-char-a sub-char-b))
  (if (and (eq (read-char stream t nil t) #\s)
           (eq (read-char stream t nil t) #\x))
      (jsx-rewrite (read stream t nil t))
      (error "Unrecognized jsx trigger. Use #jsx(...).")))
      
(defvar *readtable-stack* nil)  

(defun enable-jsx-reader ()
  (push *readtable* *readtable-stack*)
  (set-dispatch-macro-character #\# #\j #'jsx-reader))

(defun disable-jsx-reader ()
  (setq *readtable* (pop *readtable-stack*)))

(defun get-initial-state (states)
  `(lambda ()
     (create ,@(loop for state in states
                  append state))))

(defun process-members (members)
  (labels ((transform-member (member)
             (case (car member)
               (t (cons (regular-symbol (car member))
                        (cdr member))))))
    (mapcan #'transform-member members)))


(defmacro def-widget (name (&rest states) (&rest members) &body body)
  `(defun ,name ()
     `(defvar ,',name ((@ *react create-class)
                       (create get-initial-state ,',(get-initial-state states)
                               ,@',(process-members members)
                               render (lambda ()
                                        ,@',body))))))
                                    


