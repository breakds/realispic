;;;; def-app.lisp

(in-package #:breakds.realispic)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun widget-dependency-closure (initial-set)
    (labels ((bfs (candidates closure)
	       (if (null candidates)
		   closure
		   (let ((new-candidates (cdr candidates))
			 (new-closure closure))
		     (loop for dep in (cdr (gethash (car candidates)
						    *realispic-symbol-table*))
			unless (member dep closure :test #'string-equal) do
			  (push dep new-closure)
			  (push dep new-candidates))
		     (bfs new-candidates new-closure)))))
      (bfs initial-set initial-set)))
  
  (defun match-file-type (url &rest types)
    (some (lambda (x) (string-equal (pathname-type url) x))
	  types)))


(defmacro def-app (app-name args 
		   &key
		     (title "realispic app")
		     (uri "/app")
		     (port 8000)
		     ;; TODO(breakds): document-base should be
		     ;; relative to the current package
		     (document-base "")
		     (icon "")
		     (includes nil)
		     (widget nil))

  (multiple-value-bind (body required-widgets body-css)
      (compile-psx widget :psx-only t)
    ;; Check Dependencies
    (let ((all-dependencies (widget-dependency-closure required-widgets)))
      (loop for widget-name in all-dependencies
	 unless (gethash widget-name *realispic-symbol-table*)
	 do (error "Compile app failed: Undefined widget :~a." widget-name))
      (print all-dependencies)
      (with-ps-gensyms (app-var)
	`(eval-when (:compile-toplevel :load-toplevel :execute)
	   ;; Define the Hunchentoot handler
	   (hunchentoot:define-easy-handler (,(symb app-name '-handler) :uri ,uri) ,args
	     (setf (hunchentoot:content-type*) "text/html")
	     (let ((html-template:*string-modifier* #'identity))
	       (with-output-to-string (html-template:*default-template-output*)
		 (html-template:fill-and-print-template
		  (merge-pathnames "template/simple-template.tmpl"
				   (asdf:system-source-directory 'realispic))
		  (list :title ,title
			:css-include ',(mapcar #`(:url ,x1)
                                               (remove-if-not #`,(match-file-type x1 "css")
                                                              includes))
			:js-include `((:url "https://ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js")
                                      (:url "https://cdnjs.cloudflare.com/ajax/libs/react/0.13.3/react-with-addons.js")
                                      ,@',(mapcar #`(:url ,x1)
                                                  (remove-if-not #`,(match-file-type x1 "js")
                                                                 includes)))
			:icon ,icon
                        :css (compile-css (quote ,(append body-css
                                                          (loop for widget-name in all-dependencies
                                                             append (funcall (car (gethash widget-name
                                                                                           *realispic-symbol-table*))
                                                                             :css)))))
			:javascript (ps* (list 'progn
					       ;; http parameters as global variables.
					       ,@(mapcar (lambda (arg)
							   `(list 'defvar 
								  ',arg
								  ,arg))
							 args)
					       ;; The widgets.
					       ,@(loop for widget-name in all-dependencies
						    collect `(quote ,(funcall 
								      (car (gethash widget-name
										    *realispic-symbol-table*))
                                                                      :javascript)))
					       ;; Place the final widget.
					       '(let ((,app-var ,body))
						 ((@ *react render)
						  ,app-var
						  ((@ document get-element-by-id) "content"))))))))))
	   ;; Define the server controller closure
	   (let ((status :stopped)
		 (acceptor (make-instance 'hunchentoot:easy-acceptor
					  :port ,port
					  :document-root ,document-base))
		 (app-title ,(mkstr app-name)))
	     ;; No need for gensym. The variable above is only used for
	     ;; the almost-static function defined below.
	     (handler-case (when (symbol-function ',app-name)
			     (funcall #',app-name :stop))
	       (t (e) (print e)))
	     (defun ,app-name (&optional (command :start))
	       (labels ((start-app ()
			  (if (eq status :running)
			      (format t "~a is already running.~%" app-title)
			      (progn (format t "Starting ~a ...~%" app-title)
				     (hunchentoot:start acceptor)
				     (setf status :running)
				     (format t "~a started.~%" app-title))))
			(stop-app ()
			  (when (eq status :running)
			    (format t "Stopping ~a ...~%" app-title)
			    (hunchentoot:stop acceptor)
			    (setf status :stopped)
			    (format t "~a stopped.~%" app-title)))
			(restart-app ()
			  (stop-app)
			  (start-app))
			(query-status ()
			  (format t "~a status: [~a]~%" app-title status)
			  status))
		 (case command
		   (:start (start-app))
		   (:stop (stop-app))
		   (:reload (restart-app))
		   (:status (query-status))
		   (t (error "wrong parameter.")))))))))))
