;;;; core.lisp
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.realispic)


(declaim (inline regular-symbol))
(defun regular-symbol (keyword-symbol)
  (symb (mkstr keyword-symbol)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun jsx-rewrite (jsx-expr)
    (let ((dom-tags '(:html :body :head :div :button :br
                      :section
                      :form :ul :li :input :span :table
                      :tr :td :button :a :h1 :h2 :h3))) 
      (labels ((transform-attribute (attribute)
                 (let ((attr-name (mkstr (car attribute))))
                   (cond ((string-equal attr-name "ref") 
                          (list (car attribute)
                                (let ((transformed (ps* (symb (cadr attribute)))))
                                  (subseq transformed 
                                          0 (1- (length transformed))))))
                         (t attribute)))))
        (cond ((atom jsx-expr) jsx-expr)
              ((keywordp (car jsx-expr))
               `(,(cond ((member (car jsx-expr) dom-tags)
                         `(@ *react 
                             *dom* 
                             ,(regular-symbol (car jsx-expr))))
                        (t (regular-symbol (car jsx-expr))))
                  (create ,@(mapcan #'transform-attribute 
                                    (second jsx-expr)))
                  ,@(mapcar #'jsx-rewrite (cddr jsx-expr))))
              (t (cons (car jsx-expr) 
                       (mapcar #'jsx-rewrite (cdr jsx-expr)))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun jsx-reader (stream sub-char-a sub-char-b)
    (declare (ignorable sub-char-a sub-char-b))
    (if (and (eq (read-char stream t nil t) #\s)
             (eq (read-char stream t nil t) #\x))
        (jsx-rewrite (read stream t nil t))
        (error "Unrecognized jsx trigger. Use #jsx(...)."))))
      
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
             (cond ((>= (length member) 3)
                    `(,(car member) (lambda ,(cadr member)
                                      ,@(cddr member))))
                   (t (cons (regular-symbol (car member))
                            (cdr member))))))
    (mapcan #'transform-member members)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *realispic-symbol-table* (make-hash-table :test #'equal)))

(defmacro def-widget (name (&rest states) (&rest members) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
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
  (with-ps-gensyms (result-symbol)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
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
                    :libs ',(mapcar #`(:url ,x1) libs)
                    :javascript (ps ,@(loop for val being the 
                                         hash-values of *realispic-symbol-table*
                                         collect (funcall val))
                                    (let ((,result-symbol (progn ,@body)))
                                      ((@ *react render-component)
                                       ,result-symbol
                                       ((@ document get-element-by-id) "content")))))))))
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
             
             
       
                                               
                                             
