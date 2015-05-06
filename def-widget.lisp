;;;; def-widget.lisp
;;;; Author: BreakDS <breakds@gmail.com>

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
  
  (defun split-argument-list (args)
    "Split the argument list, so that we know which of the agruments
    are atrributes (properties in React terms), and which are states."
    (let (attributes states)
      (loop for arg in args
         do (cond ((eq (cadr arg) :attribute)
                   (push arg attributes))
                  ((eq (cadr arg) :state)
                   (push arg states))
                  (t (error (mkstr "~a is not a recognizable label of arguments. "
                                   "Valid labels are  :atrribute and :state.")
                            (cadr arg)))))
      (values attributes states)))

  (defun initial-state-slot (states)
    "Return the parenscript code that defines the get-initial-state
    slot in React's create-class input."
    `(lambda ()
       (create ,@(loop for state in states
                      append (list (car state)
                                   ;; remove :state
                                   (caddr state))))))

  (defun unquantify-keyword (keyword)
    "Convert a keyword into its counter-part without the leading :."
    (symb (mkstr keyword)))
  
  (defun compile-psx (expr attribute-names)
    (labels ((try-replace-with-local-attribute (expr)
               (if (atom expr)
                   (if (and (symbolp expr)
                            (member (symbol-name expr)
                                    attribute-names :test #'string-equal))
                       `(@ this props ,expr)
                       expr)
                   (mapcar #'try-replace-with-local-attribute expr)))
             (transform-attribute (attribute)
               (destructuring-bind (name value) attribute
                 (cond ((string-equal name "style")
                        `(,name (create ,@(mapcan 
                                           (lambda (style-name style-value)
                                                    ;; TODO(breakds):
                                                    ;; make sure
                                                    ;; style-name is a
                                                    ;; keyword.
                                             (ecase style-name 
                                               (:z-index (list 'z-index 
                                                               style-value))
                                               (t (list style-name style-value))))
                                           ;; TODO(breakds): Check
                                           ;; whether value is pair of
                                           ;; 2.
                                           (group value 2)))))
                       (t (list name 
                                (try-replace-with-local-attribute value)))))))
      (cond ((atom expr) (try-replace-with-local-attribute expr))
            ((keywordp (car expr))
             ;; Keyword case, can be either a standard html tag, or a custom tag.
             ;; Call React.DOM.tag-name when it is a standard html-tag, or the custom
             ;; ReactClass constructor otherwise.
             ;; TODO(breakds): Compile time error if tag is not recognizable.
             `(,(if (member (car expr) *html-tags*)
                    `(@ *react *dom* (unquantify-keyword (car expr)))
                    (unquantify-keyword (car expr)))
                ;; Handle input-attributes provided for this tag.
                ;; Note that we DO NOT allow for PSX syntax in
                ;; attributes.
                ;;
                ;; This is understandable because we never put html
                ;; code inside html attributes.
                (create ,@(second expr))
                ,@(loop for sub-expr in (cddr expr)
                     collect (compile-psx sub-expr attribute-names))))
            (t ;; In this case, must be a function call/macro
               ;; call/speacial operator call.
             (cons (try-replace-with-local-attribute (car expr))
                   (loop for sub-expr in (cdr expr)
                      collect (compile-psx sub-expr attribute-names))))))))

(defmacro def-widget-1 (name (&rest args) &body body)
  (multiple-value-bind (attributes states)
      (split-argument-list args)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,name ()
         `(defvar ,',name ((@ *react create-class)
                           (create get-initial-state ,',(initial-state-slot states)
                                   render ,',(compile-psx body 
                                                          (mapcar #`,(symbol-name (car x1))
                                                                  attributes))))))
       (setf (gethash (ps:ps-inline ,name) *realispic-symbol-table*)
             #',name))))
                           



                                   
            

