;;;; rpc.lisp
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.realispic)

(defun to-js-name (ps-symb)
  (let ((transformed (ps* ps-symb)))
    (subseq transformed 0 (1- (length transformed)))))

(defmacro def-rpc (name args &body body)
  (with-gensyms (json-obj session)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (hunchentoot:define-easy-handler (,(symb name '-service) :uri ,(mkstr "/" (to-js-name name))) ()
         (setf (hunchentoot:content-type*) "application/json")
         (let (;;(,json-obj (jsown:parse (hunchentoot:raw-post-data :force-text t)))
               (,session (hunchentoot:start-session)))
           (declare (ignorable ,session))
         (hunchentoot:log-message* :info "bingo!"))))))

(defpsmacro with-rpc ((result (name &rest args)) &body body)
  `(chain $ (ajax (create url ,(mkstr "/" (to-js-name name))
                          data-type "json"
                          type "POST"
                          data (create ,@args)
                          success (chain (lambda (,result)
                                           ,@body)
                                         (bind this))
                          error (chain (lambda (xhr status err)
                                         (chain console (error (chain err (to-string)))))
                                       (bind this))))))
                          
  
           
           
           
         
         


