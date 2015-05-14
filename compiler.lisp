;;;; compiler.lisp
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.realispic)


;; TODO(breakds): compiler utils should live in antoher file,
;; e.g. compiler-utils.lisp.
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

  (defun match-symbol (input-form &optional (symbol-name nil))
    (and (symbolp input-form)
         (not (keywordp input-form))
         (if symbol-name
             (string-equal (symbol-name input-form) symbol-name)
             t)))

  (defun match-&-symbol (input-form &optional (symbol-name nil))
    (and (symbolp input-form)
         (not (keywordp input-form))
         (eq (char (symbol-name input-form) 0) #\&)
         (if symbol-name
             (string-equal (symbol-name input-form) symbol-name)
             t)))

  ;; Example Matchers:
  ;; 1) ((:symbol "lambda") args &rest body)
  ;; 2) ((:symbol "let") bindings &rest body)
  ;; 3) ((:keyword "html") attributes 
  (defun compile-matcher (name pattern body)
    (with-gensyms (shadowed)
      `(,name (form ,shadowed matcher-list)
              ;; Destructring bind the input form into the pattern.
              (let ((shadowed ,shadowed))
                ,(if pattern
                     (let ((lambda-list 
                            (loop for entry in pattern
                               collect (cond ((match-&-symbol entry "&rest")
                                              entry)
                                             ((and (match-symbol entry)
                                                   (not (match-&-symbol entry)))
                                              entry)
                                             ((and (listp entry)
                                                   (match-symbol (car entry))
                                                   (not (match-&-symbol (car entry))))
                                              (car entry))
                                             (t (error "compile-matcher: failed to parse pattern ~a."
                                                       pattern))))))
                       `(and 
                         ;; first try destructuring-bind, if it fails, it
                         ;; suggests "no match".
                         (handler-case 
                             (destructuring-bind ,lambda-list form
                               (declare (ignore ,@(remove-if #`,(match-&-symbol x1) lambda-list)))
                               t)
                           (t () nil))
                         ;; If pass the destructuring-bind test, safely
                         ;; proceed.
                         (destructuring-bind ,lambda-list form
                           ;; Evaluate the condition enforced by the pattern
                           (when (and ,@(loop for entry in pattern
                                           when (listp entry)
                                           collect (cond ((eq (second entry) :symbol)
                                                          ;; dicatate the bound value
                                                          ;; is a symbol, or is a
                                                          ;; symbol with the specified name
                                                          (case (length entry)
                                                            (2 `(match-symbol ,(car entry)))
                                                            (3 `(match-symbol ,(car entry) ,(third entry)))
                                                            (t (error (mkstr "compile-matcher: "
                                                                             "Too many specifiers in ~a.")
                                                                      entry))))
                                                         ((eq (second entry) :keyword)
                                                          (if (= (length entry) 2)
                                                              `(keywordp ,(car entry))
                                                              (error (mkstr "compile-matcher: "
                                                                            "Too many specifiers in ~a.")
                                                                     entry)))
                                                         (t (error (mkstr "compile-matcher: "
                                                                          "Invalid entry ~a.")
                                                                   entry)))))
                             ,@body))))
                     `(when (atom form)
                        ,@body)))))))

(defmacro def-code-walker (name extra-args (&rest matchers) &body body)
  (with-gensyms (form shadowed sub-form enabled-matchers matcher result args)
    `(defun ,name (form &key ,@extra-args)
       (macrolet ((process (,form &rest ,args)
                    `(execute ,,form shadowed matcher-list ,@,args))
                  (process-each (,form &rest ,args)
                    `(loop for ,',sub-form in ,,form
                        collect (execute ,',sub-form shadowed matcher-list ,@,args)))
		  (push-shadowed (&rest ,args)
		    `(progn ,@,(list 'mapcar '#`(push (symbol-name ,x1) shadowed)
				     args))))
         (labels ((execute (,form ,shadowed ,enabled-matchers &key (on nil) (off nil))
                    (or (loop 
                           for ,matcher in (remove-if #`,(member x1 off)
						      (union ,enabled-matchers on))
                           for ,result = (funcall ,matcher ,form ,shadowed ,enabled-matchers)
                           when ,result
                           return ,result)
			;; If nothing matches, fall back to the
			;; default cases.
			(and (atom ,form) ,form)
			(loop for ,sub-form in ,form
			   collect (execute ,sub-form ,shadowed ,enabled-matchers))))
                  (initialize (&rest ,enabled-matchers)
                    (funcall #'execute form nil ,enabled-matchers))
                  ,@(mapcar #`,(compile-matcher (car x1)
                                                (second x1)
                                                (cddr x1))
                            matchers))
           
           ,@body)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun one-of-symbols-p (input-form symbol-names)
    (and (symbolp input-form)
         (member (symbol-name input-form) symbol-names
                 :test #'string-equal)))

  (defun unquantify-keyword (keyword)
    "Convert a keyword into its counter-part without the leading ':'."
    (symb (mkstr keyword))))

(def-code-walker compile-psx (attribute-names)
    ((atom-attribute () (when (and (one-of-symbols-p form attribute-names)
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
     (psx-tags ((tag :keyword) attributes &rest body) 
               ;; Keyword case, can be either a standard html tag, or
               ;; a custom tag.  Call React.DOM.tag-name when it is a
               ;; standard html-tag, or the custom ReactClass
               ;; constructor otherwise.  TODO(breakds): Compile time
               ;; error if tag is not recognizable.
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
                                      (list (car attribute-pair)
                                            (process (cadr attribute-pair) 
						     :off `(,#'psx-tags))))
                                    attributes))
                  ,@(process-each body))))
  (initialize #'atom-attribute #'let-form #'let*-form #'lambda-form 
	      #'psx-tags))
                                 
                                 
                     


                     
