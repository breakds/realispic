;;;; def-app.lisp

(in-package #:breakds.realispic)

(defmacro def-app ((app-name arguments &key 
			     (title "realispic app")
			     (uri "/app")
			     (css nil)
			     (icon "")
			     (libs nil)
			     (port 8000)
			     (document-base "")) &body body)
  (with-ps-gensyms (result-symbol)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (hunchentoot:define-easy-handler (,(symb app-name '-handler) :uri ,uri) ,arguments
         (setf (hunchentoot:content-type*) "text/html")
         (let ((html-template:*string-modifier* #'identity))
           (with-output-to-string (html-template:*default-template-output*)
             (html-template:fill-and-print-template
              (merge-pathnames "template/simple-template.tmpl"
                               (asdf:system-source-directory 'realispic))
              (list :title ,title
                    :css ',(mapcar #`(:url ,x1) css)
                    :libs ',(mapcar #`(:url ,x1) libs)
                    :icon ,icon
                    :javascript (ps* (append 
                                      (list 'progn 
                                            ,@(mapcar (lambda (arg)
                                                        `(list 'defvar 
                                                               ',arg
                                                               ,arg))
                                                      arguments))
                                      '(,@(loop for val being the 
                                             hash-values of *realispic-symbol-table*
                                             collect (funcall val))
                                        (let ((,result-symbol (progn ,@body)))
                                          ((@ *react render-component)
                                           ,result-symbol
                                           ((@ document get-element-by-id) "content")))))))))))
       (let ((status :stopped)
             (acceptor (make-instance 'hunchentoot:easy-acceptor
                                      :port ,port
                                      :document-root ,document-base))
             (app-title ,(mkstr app-name)))
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
                      (format t "~a status: [~a]~%" app-title status)))
             (case command
               (:start (start-app))
               (:stop (stop-app))
               (:reload (restart-app))
               (:status (query-status))
               (t (error "wrong parameter.")))))))))
