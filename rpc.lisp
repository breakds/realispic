;;;; rpc.lisp
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.realispic)

(defun to-js-name (ps-symb)
  "Convert the symbol to its corresponding javascript name."
  ;; Examples:
  ;; (to-js-name 'my-name) => "myName"
  ;; (to-js-name '*upper-case*) => "UPPER-CASE"
  (let ((transformed (ps* ps-symb)))
    (subseq transformed 0 (1- (length transformed)))))

(defun to-js-keyword (ps-symb)
  "Convert the keyword PS-SYMB in to a javascript name."
  (let ((transformed (ps* ps-symb)))
    (subseq transformed 1 (- (length transformed) 2))))

;;; DEF-RPC is a macro that can be used to define an RPC service. The
;;; usage is exacly the same as DEFUN, and it will result in a
;;; top-level function with the given NAME just as what DEFIN
;;; does. One extra thingthat DEF-RPC does is that it will provide a
;;; Hunchentoot handler registerd at "/{NAME}" and can be called into
;;; with parenscript macro WITH-RPC. See below for comment of
;;; WITH-RPC.

(defmacro def-rpc (name args &body body)
  "This macro is used to define an RPC function."
  (with-gensyms (json-input session unmarshalled keyword-arg)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,name (current-session ,@args)
	 ,@body)
       (hunchentoot:define-easy-handler (,(symb name '-service) 
					  :uri ,(mkstr "/" 
						       (to-js-name name))) ()
         (setf (hunchentoot:content-type*) "application/json")
         (let ((,json-input (jsown:parse (hunchentoot:raw-post-data 
					  :force-text t)))
               (,session (hunchentoot:start-session)))
           (declare (ignorable ,session))
	   (hunchentoot:log-message* :info "~a" ,json-input)
	   (handler-case
	       (jsown:to-json 
		(apply #',name 
		       (append (cons ,session (jsown:val ,json-input
							 "plain-parameters"))
			       (append 
				,@(loop for key-arg
				     in (awhen (position '&key args)
					  (nthcdr (1+ it) args))
				     for key-name = (if (atom key-arg)
							key-arg
							(car key-arg))
				     collect 
				       `(handler-case 
					    (list ,(mk-keyword key-name)
						  (jsown:val ,json-input
							     ,(to-js-keyword
							       (mk-keyword
								key-name))))
					  (t () nil)))))))
	     (t (e) (error "server side rpc failure ~a" e))))))))
  

(defun gen-json-marshall-ps (args)
  "Generate the JSON representation of the args. This is an aux
  function for WITH-RPC."
  (let ((keyword-start (aif (position-if #'keywordp args)
			    it
			    (length args))))
    `(chain *json* 
	    (stringify (create :plain-parameters 
			       (array ,@(subseq args 0 keyword-start))
			       ,@(progn
				  (loop for pair in (group (nthcdr keyword-start
								   args)
							   2)
				     when (not (keywordp (car pair)))
				     do (error "bad keyword argument pairs: ~a"
					       args))
				  (nthcdr keyword-start args)))))))

;;; WITH-RPC is a parenscript macro to call to an rpc service. See the
;;; example below:
;;; 
;;; (with-rpc (remote-plus "12" "13" :base 10)
;;;    (chain console (log rpc-result)))
;;;
;;; where REMOTE-PLUS is the name of a defined rpc service. The syntax
;;; for calling REMOTE-PLUS is exactly the same as calling a normal
;;; lisp function where :base specifies the keyword argument. The
;;; result will be stored in the variable RPC-RESULT.

(defpsmacro with-rpc ((name &rest args) &body body)
  `(chain $ (ajax (create url ,(mkstr "/" (to-js-name name))
                          data-type "json"
                          type "POST"
                          data ,(gen-json-marshall-ps args)
                          success (chain (lambda (rpc-result)
                                           ,@body)
                                         (bind this))
                          error (chain (lambda (xhr status err)
                                         (chain console (error (chain err (to-string)))))
                                       (bind this))))))
                          
  
           
           
           
         
         


