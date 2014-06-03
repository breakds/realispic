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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *realispic-symbol-table* (make-hash-table :test #'equal)))

(defmacro def-widget (name (&rest states) (&rest members) &body body)
  `(progn
     (defun ,name ()
       `(defvar ,',name ((@ *react create-class)
                         (create get-initial-state ,',(get-initial-state states)
                                 ,@',(process-members members)
                                 render (lambda ()
                                          ,@',body)))))
     (setf (gethash (ps:ps-inline ,name) *realispic-symbol-table*) #',name)))

(defmacro def-realispic-app ((app-name &key 
                                       (title "realispic app")
                                       (template nil)
                                       (uri "/app")
                                       (css nil)
                                       (libs nil)
                                       (port 8000)
                                       (document-base "")) &body body)
  `(progn
     (hunchentoot:define-easy-handler (,(symb app-name '-handler) :uri ,uri) ()
       (setf (hunchentoot:content-type*) "text/html")
       (let ((html-template:*string-modifier* #'identity))
         (with-output-to-string (html-template:*default-template-output*)
           (html-template:fill-and-print-template
            ,(if template 
                 template 
                 (merge-pathnames "template/simple-template.tmpl"
                                  (asdf:system-source-directory 'realispic)))
            (list :title ,title
                  :css ',(mapcar #`(:url ,x1) css)
                  :libs ',(mapcan #`(:url ,x1) libs)
                  :javascript (ps* ,@body))))))
     (defun ,app-name (&optional (command :start))
       (let ((status :stopped)
             (acceptor (make-instance 'hunchentoot:easy-acceptor
                                      :port ,port
                                      :document-root ,document-base))
             (app-title ,(mkstr app-name)))
         (labels ((start-app ()
                    (if (eq status :running)
                        (format t "~a is already running.~%" app-title)
                        (progn (format t "Starting ~a ...~%" app-title)
                               (hunchentoot:start acceptor)
                               (setq status :running)
                               (format t "~a started.~%" app-title))))
                  (stop-app ()
                    (when (eq status :running)
                      (format t "Stopping ~a ...~%" app-title)
                      (hunchentoot:stop acceptor)
                      (setq status :stopped)
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
             (t (error "wrong parameter."))))))))
             
             
       
                                               
                                             
