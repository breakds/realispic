;;;; css.lisp
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.realispic)

;;; Currently using a simplified CSS syntax.
;;;
;;; CSS-RULE = (CSS-SELECTOR 

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun gen-keyframes (name keyframes)
    `(:keyframes :name ,name
		 :
    (fresh-line stream)
    (format stream "~akeyframes ~a {~%"
	    prefix name)
    (loop for keyframe in keyframes do
	 (cond ((= (length keyframe) 3)
		(format stream "  ~a { ~a: ~a; }~%"
			(car keyframe) (cadr keyframe) (caddr keyframe)))
	       (t (format stream "  ~a {~%" (car keyframe))
		  (loop for (style-name style-value) on (cdr keyframe)
		     by #'cddr do
		       (format stream "    ~a: ~a;~%"
			       style-name style-value))
		  (format stream "  }~%"))))
    (format stream "}~%~%")))

  ;; (defun process-animation (name instructions)
  ;;   (let ((output (make-string-output-stream))
  ;; 	  keyframes properties)
  ;;     ;; classify keyframes and properties
  ;;     (loop for (directive value) on instructions by #'cddr
  ;; 	 collect (case directive
  ;; 		   (:keyframe (push value keyframes))
  ;; 		   (t (push (list directive value) properties))))

  
  
