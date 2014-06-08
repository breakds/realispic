;;;; rpc.lisp
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.realispic)

(defun to-js-name (ps-symb)
  (let ((transformed (ps* ps-symb)))
    (subseq transformed 0 (1- (length transformed)))))

(defun to-js-keyword (ps-symb)
  (let ((transformed (ps* ps-symb)))
    (subseq transformed 1 (- (length transformed) 2))))

(defmacro def-rpc (name args &body body)
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
                          
  
           
           
           
         
         


