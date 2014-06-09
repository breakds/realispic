;;;; core.lisp
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.realispic)

;;; ---------- Utilities ----------

(declaim (inline regular-symbol))
(defun regular-symbol (keyword-symbol)
  "Returns the corresponding regular symbol for a keyword symbol."
  ;; Examples:
  ;; (regular-symbol :a) => 'A
  ;; (regular-symbol :ab-cd) => 'AB-CD
  (symb (mkstr keyword-symbol)))


;;; ---------- JSX reader ----------
;;;
;;; The following macros and subroutines provides the reader macro for
;;; reading JSX syntax (See
;;; http://facebook.github.io/react/docs/displaying-data.html#jsx-syntax
;;; for details). JSX syntax in Realispic are recognized as code
;;; enclosed within #jsx(). For example:
;;;
;;; #jsx(:a ((href "/about")
;;;          (class-name "link"))
;;;         "click me")
;;; represents equivalent JSX code in javacript as
;;;
;;; <a href="/about" className="link">click me</a>
;;; 
;;; A #jsx form consists of 3 parts, the tag/class name (which should
;;; always be a keyword, the attributes/properties which are key-value
;;; pairs and the children. This is very similar to the original React
;;; JSX syntax. However, you do not have to use "{...}" to escape non
;;; jsx code in #jsx syntax since the reader here is aware of code
;;; that are in raw parenscript.
;;; 
;;; 
;;; Also, it's a good habit to enclose the code using #jsx syntax
;;; between enable-jsx-reader and disable-jsx-reader. For example:
;;; 
;;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;;  (enable-jsx-reader))
;;; 
;;; (def-widegt widget-a
;;;   ....
;;;   #jsx(...))
;;;
;;; (def-widegt widget-b
;;;   ....
;;;   #jsx(...))
;;;
;;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;;  (disable-jsx-reader))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun jsx-rewrite (jsx-expr)
    "This function is used to rewrite the JSX experssion as a React
    parenscript expression."
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
	       ;; dom-tags are html tags natively supported by React.DOM
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
    "The reader for JSX syntax."
    (declare (ignorable sub-char-a sub-char-b))
    (if (and (eq (read-char stream t nil t) #\s)
             (eq (read-char stream t nil t) #\x))
        (jsx-rewrite (read stream t nil t))
        (error "Unrecognized jsx trigger. Use #jsx(...)."))))
      
(defvar *readtable-stack* nil)  

(defun enable-jsx-reader ()
  "Call this function to enable JSX syntax."
  (push *readtable* *readtable-stack*)
  (set-dispatch-macro-character #\# #\j #'jsx-reader))

(defun disable-jsx-reader ()
  "Call this function to disable JSX syntax."
  (setq *readtable* (pop *readtable-stack*)))


;;; ---------- Widgets ----------
;;; 
;;; The following code provide macros to define a React widget. An
;;; example of defining a widget is provided below:
;;; 
;;; 1   (def-widget comment (author comment-content)
;;; 2       ((state (color "#ff0000")
;;; 3               (size 10))
;;; 4        (change-color (new-color)
;;; 5           (chain this (set-state (create color new-color)))))
;;; 6     #jsx(:div ()
;;; 7               (:h3 ((style (create "font-color" (local-state color))))
;;; 8                    author)
;;; 9               (:div ((style (create "font-size" (local-state size))))
;;; 10                    comment-content)))
;;;
;;; In the above example, we define a widget with 4 parts:
;;;
;;; (1) In line 1, we specify that the name of the widget is "comment"
;;;
;;; (2) In line 2, we specify that the widget "comment" has two
;;;     properties (props in React), author and comment-content. This
;;;     means that when trying to create a new "comment" object, we
;;;     can use
;;;     #:jsx(:comment ((author "Batman")
;;;                     (comment-content "I must protect the city.")))
;;;     The two props can be used as they are in the rest of the code,
;;;     which is analogical to function arguments.
;;;
;;; (3) Line 2 - 5 defines the members of the widget. 
;;;
;;;     Line 2 - 3 defines a special member state, which is followed
;;;     by a list of key value pairs. Those key value pairs are used
;;;     to define the states and setting their initial values.
;;;     
;;;     Line 4 - 5 defines a member function change-color. This
;;;     function accepts argument specified in the lambda-form
;;;     (new-color). The rest code on line 5 is the body of the
;;;     function which updates the state "color" with the argument
;;;     new-color.
;;;
;;; (4) Line 6 - 10 is the body of the widget, which defines how the
;;;     widget is going to be rendered as an DOM object (See JSX
;;;     syntax above to understand the details for the syntax
;;;     here). Note that we directly use props author and
;;;     coment-content here to access the props, and use parenscript
;;;     macro local-state to read the value of the state.


(defun get-initial-state (states)
  "Rewrite the parenscript to generate the get-initial-state member
  function in parenscript for the key-value pairs in STATES."
  `(lambda ()
     (create ,@(loop for state in states
                  append state))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *realispic-symbol-table* (make-hash-table :test #'equal)))

(defun exist-props-violations (props)
  "check whether the properties are all symbols which do not
  conflict with symbols defined in ps:*definied-operators*"
  (or
   (awhen (remove-if #'symbolp props)
     (error "Realispic: expects all props to be symbols. Please check~{ ~a~}."
	    it)
     t)
   (awhen (remove-if-not #`,(member x1 *defined-operators*) props)
     (error "Realispic: ~{~a ~}conflicts with names in ps:*defined-operators*."
	    it)
     t)))

(defun replace-props (props expr)
  "Recognize symbols that actually denote properties, and replace them
  with the form (@ this props the-symbol)."
  (cond ((and (symbolp expr)
	      (member expr props))
	 `(@ this props ,expr))
	((consp expr) (mapcar #`,(replace-props props x1) expr))
	(t expr)))

(defun process-members (members props)
  "Process the members of a widget definition form, handling special
  forms like state and functions."
  (labels ((transform-member (member)
             (cond ((string-equal (car member) "state")
		    `(get-initial-state 
		      ,(get-initial-state (replace-props props 
							 (cdr member)))))
		   ((>= (length member) 3)
		    `(,(car member) (lambda ,(cadr member)
				      ,@(replace-props props 
						       (cddr member)))))
		   (t (cons (regular-symbol (car member))
                            (cdr (replace-props props member)))))))
    (mapcan #'transform-member members)))

(defmacro def-widget (name (&rest props) (&rest members) &body body)
  "The interface to call to define a widget. This will have the side
  effect of defning a top-level function with NAME, and have it
  registered in *realispic-symbol-table*. Calling this function will
  produce parenscript code to define the widget (React class). See
  section comment for usage."
  (when (not (exist-props-violations props))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,name ()
	 `(defvar ,',name ((@ *react create-class)
			   (create ,@',(process-members members props)
				   render (lambda ()
					    ,@',(replace-props props 
							       body))))))
       (setf (gethash (ps:ps-inline ,name) 
		      *realispic-symbol-table*) #',name))))


;;; ---------- Application ----------
;;;
;;; The macro def-realispic-app is used to define a web application
;;; based on realispic. Below is an example:
;;;
;;; 1   (def-realispic-app (my-game :title "My App"
;;; 2                               :port 12345
;;; 3                               :uri "/myapp"
;;; 4                               :css ("assets/css/topcoat.css")
;;; 5                               :libs ("assets/js/jquery.js"
;;; 6                                      "assets/js/react.js")
;;; 7                               :document-base "~/www/myapp")
;;; 8      #jsx(:game-panel))
;;;
;;; Where,
;;; 
;;; Line 1 specifies the name of the app as MY-GAME, and the
;;;        displayed title of the app is going to be "My App".
;;;
;;; Line 2 - 3 specifies the uri of the app to be "/myapp", and the
;;;            port that listens requests for the app is 12345. This
;;;            means that if the app is launched locally, you can
;;;            access the app via browswer by linking to
;;;            http://localhost:12345/myapp.
;;;
;;; Line 4 specifies the CSS files to link for the app. 
;;;
;;; Line 5 - 6 specifies the javascript libs to link for the
;;;            app. Remember to include "react.js". This may not be
;;;            necessary in the following versions of realispic as I
;;;            plan to have it checked/included automatically.
;;;        
;;; Line 7 specifies the location for the app to look for static
;;;        files.
;;;
;;; Line 8 is the body of the app. It will place the instance of the
;;;        widget GAME-PANEL in the webpage.
;;;
;;; Defining the app MY-GAME implies defining a function called
;;; MY-GAME-APP. It can be called to fulfilled diffrent
;;; functionalities:
;;;
;;; (1) (my-game-app :start) => start the server
;;; (2) (my-game-app :stop) => stop the server
;;; (3) (my-game-app :status) => check the status of the server
;;; (4) (my-game-app :reload) => stop and restart the server

(defmacro def-realispic-app ((app-name &key 
                                       (title "realispic app")
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
              (merge-pathnames "template/simple-template.tmpl"
                               (asdf:system-source-directory 'realispic))
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
             
             
       
                                               
                                             
