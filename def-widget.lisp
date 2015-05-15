;;;; def-widget.lisp
;;;; Author: BreakDS <breakds@gmail.com>

;;; TODO(breakds): more compiler error prompts with condition system.

(in-package #:breakds.realispic)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *html-tags* 
    '(:html :body :head :span ;; Structure
      :p :h1 :h2 :h3 :h4 :h5 :h6 :br ;; Text
      :a ;; Links
      :div :frame :iframe :form :figure :img :video ;; Container
      :table :tbody :tr :td :th :thead :tfoot :caption;; Table
      :ul :ol :li ;; List
      :input :textarea :select :option :optgroup :button :label :fieldset :legend ;; Interaction
      :script ;; Script
      :b :i :sub :sup :big :small :hr) ;; Style
    "All the keywords that will be recognized as standard html tags by compile-psx.")

  (defvar *realispic-symbol-table* (make-hash-table :test #'equal)
    "Stores all the realispic (custom) symbols and its value, where
    the value is the function that generates ParenScript code.")


  (defmacro compile-error ((&rest format-string) &rest args)
    `(error (mkstr "Realispic Compilation Error: " 
                   ,@format-string)
            ,@args))

  (defun split-argument-list (args)
    "Split the argument list, so that we know which of the agruments
    are atrributes (properties in React terms), and which are states."
    (let (attribute-names state-defs)
      (loop for arg in args
         do (cond ((not (listp arg))
                   (compile-error ("Expect a list for argument definition, "
                                   "but get ~a.")
                                  arg))
                  ((not (symbolp (car arg)))
                   (compile-error ("~a is defined as an argument in ~a, but "
                                   "is not a symbol.")
                                  (car arg) arg))
                  ((eq (cadr arg) :attribute)
                   (push (symbol-name (car arg)) 
                         attribute-names))
                  ((eq (cadr arg) :state)
                   (push (list (car arg) (third arg))
                         state-defs))
                  (t (compile-error ("~a is not a recognizable label of arguments. "
                                     "Valid labels are :atrribute and :state.")
                                    (cadr arg)))))
      (values attribute-names state-defs)))

  (defun initial-state-slot (state-defs)
    "Return the parenscript code that defines the get-initial-state
    slot in React's create-class input."
    `(lambda ()
       (create ,@(mapcan #'identity state-defs))))

  (defun one-of-symbols-p (input-form symbol-names)
    (and (symbolp input-form)
         (member (symbol-name input-form) symbol-names
                 :test #'string-equal)))

  (defun unquantify-keyword (keyword)
    "Convert a keyword into its counter-part without the leading ':'."
    (symb (mkstr keyword)))

  (defun process-style-name (style-name)
    style-name))

(def-code-walker compile-psx (attribute-names state-defs)
    ((atom-attribute () (when (and (atom form)
				   (one-of-symbols-p form attribute-names)
                                   (not (one-of-symbols-p form shadowed)))
                          `(@ this props ,form)))
     (let-form ((let-symbol :symbol "let") bindings &rest body)
               `(let ,(mapcar (lambda (binding)
                                (list (car binding)
                                      (process (second binding))))
                              bindings)
		  ,@(progn (mapcar (lambda (binding) (push-shadowed (car binding))) 
				   bindings)
			   nil)
                  ,@(process-each body)))
     (let*-form ((let-symbol :symbol "let*") bindings &rest body)
		`(let* ,(mapcar (lambda (binding)
				  (let ((result (list (car binding)
						      (process (second binding)))))
				    (push-shadowed (car binding))
				    result))
				bindings)
		   ,@(process-each body)))
     (lambda-form ((lambda-symbol :symbol "lambda") arg-list &rest body)
		  `(lambda ,(mapcar (lambda (arg)
				      ;; TODO(breakds): Should
				      ;; consider shadow in default
				      ;; value forms.
				      (cond ((match-&-symbol arg) arg)
					    ((match-symbol arg) 
					     (push-shadowed arg)
					     arg)
					    ((and (listp arg) (match-symbol (car arg)))
					     (push-shadowed (car arg))
					     arg)
					    (t (error "lambda-form: ~a is not a valid argument."
						      arg))))
				    arg-list)
		     ,@(process-each body)))
     (update-state-form ((fun-name :symbol "update-state") &rest pairs)
                        (let ((valid-state-names (mapcar #`,(symbol-name (car x1)) 
                                                         state-defs)))
                          `((@ this set-state) 
                            (create ,@(loop for (var-name value) 
                                         on pairs by #'cddr
                                         append (if (member (symbol-name var-name)
                                                            valid-state-names
                                                            :test #'string-equal)
                                                    (list var-name 
                                                          (process value
                                                                   :off `(,#'update-state-form
                                                                          ,#'psx-tags)))
                                                    (error (mkstr "state-ref: ~a is not a valid "
                                                                  "state name. Expect one of "
                                                                  "{~{~a~^, ~}}.")
                                                           var-name valid-state-names)))))))
     (state-ref ((state-key :keyword) var-name)
		(when (eq state-key :state)
		  (unless (match-symbol var-name)
		    (error "state-ref: expect a symbol as state name but get ~a."
			   var-name))
		  (let ((valid-state-names (mapcar #`,(symbol-name (car x1)) 
						   state-defs)))
		    (unless (member (symbol-name var-name)
				    valid-state-names
				    :test #'string-equal)
		      (error (mkstr "state-ref: ~a is not a valid state name. "
				    "Expect one of {~{~a~^, ~}}.")
			     var-name valid-state-names)))
		  `(@ this state ,var-name)))
     (psx-tags ((tag :keyword) attributes &rest body)
	       (unless (eq tag :state)
		 ;; Keyword case, can be either a standard html tag, or
		 ;; a custom tag.  Call React.DOM.tag-name when it is a
		 ;; standard html-tag, or the custom ReactClass
		 ;; constructor otherwise.  TODO(breakds): Compile time
		 ;; error if tag is not recognizable.
		 (unless (listp attributes)
		   ;; Validation of attributes.
		   (error "psx-tags: expect list as attributes for ~a but get ~a."
			  tag
			  attributes))
		 `(,(if (member tag *html-tags*)
			`(@ *react *dom* ,(unquantify-keyword tag))
			(unquantify-keyword tag))
		    ;; Handle input-attributes provided for this tag.
		    ;; Note that we DO NOT allow for PSX syntax in
		    ;; attributes.
		    ;;
		    ;; This is understandable because we never put
		    ;; html code inside html attributes.
		    (create ,@(mapcan (lambda (attribute-pair)
					(list (cond ((string-equal (car attribute-pair) 
                                                                   "class")
                                                     'class-name)
                                                    (t (car attribute-pair)))
                                              (if (string-equal (symbol-name (car attribute-pair))
                                                                "style")
                                                  `(create ,@(loop for (style-name style-value)
                                                                on (rest attribute-pair)
                                                                by #'cddr
                                                                append (list (process-style-name 
                                                                              style-name)
                                                                             (process style-value))))
                                                  (process (cadr attribute-pair) 
                                                           :off `(,#'psx-tags)))))
				      attributes))
		    ,@(process-each body))))
     (top-level () (when (or (atom form)
			     (not (match-symbol (car form) "labels")))
		     `(render (lambda () ,(process form
						   :off (list #'top-level 
							      #'top-level-labels)
						   :on (list #'atom-attribute
							     #'let-form
							     #'let*-form
							     #'lambda-form
							     #'psx-tags
                                                             #'update-state-form
							     #'state-ref))))))
     (top-level-labels ((labels-symbol :symbol "labels") fun-defs &rest body)
		       `(render 
			 (lambda ()
			   ,@(process-each body 
					   :off (list #'top-level #'top-level-labels)
					   :on (list #'atom-attribute
						     #'let-form
						     #'let*-form
						     #'lambda-form
						     #'psx-tags
                                                     #'update-state-form
						     #'state-ref)))
			 ,@(mapcan (lambda (fun-def)
				     (list (car fun-def)
					   (process (cons 'lambda (rest fun-def)) 
						    :off (list #'top-level #'top-level-labels)
						    :on (list #'atom-attribute
							      #'let-form
							      #'let*-form
							      #'lambda-form
							      #'psx-tags
                                                              #'update-state-form
							      #'state-ref))))
				   fun-defs))))
  (initialize #'top-level #'top-level-labels))

(defmacro def-widget-1 (name (&rest args) &body body)
  (multiple-value-bind (attribute-names state-defs)
      (split-argument-list args)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,name ()
         `(defvar ,',name ((@ *react create-class)
                           (create get-initial-state ,',(initial-state-slot state-defs)
                                   ,',@(compile-psx (if (and (listp body)
                                                             (= (length body) 1))
                                                        body
                                                        `(progn ,@body))
                                                    :attribute-names attribute-names
                                                    :state-defs state-defs)))))
       (setf (gethash (ps:ps-inline ,name) *realispic-symbol-table*)
             #',name))))
