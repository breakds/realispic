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
    "Convert a keyword into its counter-part without the leading ':'."
    (symb (mkstr keyword)))


  ;;; ---------- Variable Transformation ----------

  (defun transform-lambda-form (form targets shadowed transform stop)
    "Apply #'TRANSFORM on matched varibles, given that the form is
    lambda-like. This means that the form is (name (args) body)."
    ;; TODO(breakds): maybe we should transform default values as
    ;; well.
    (let* ((new-bound (remove-if #'null
                                 (mapcar (lambda (arg)
                                           (if (atom arg)
                                               (when (symbolp arg)
                                                 (symbol-name arg))
                                               (when (symbolp (car arg))
                                                 (symbol-name (car arg)))))
                                         (second form))))
           (updated-shadowed (union (intersection new-bound targets
                                                  :test #'string-equal)
                                    shadowed
                                    :test #'string-equal)))
      `(,(car form) ,(second form)
         ,@(mapcar #`,(transform-form x1 targets updated-shadowed 
                                      transform stop)
                   (cddr form)))))

  (defun transform-let-form (form targets shadowed transform stop)
    ;; TODO(breakds): Should throw error when bound variable is not a
    ;; symbol.
    (let* ((new-bound (mapcar #`,(symbol-name (car x1)) (cadr form)))
           (updated-shadowed (union (intersection new-bound targets
                                                  :test #'string-equal)
                                    shadowed
                                    :test #'string-equal)))
      `(let ,(mapcar (lambda (binding)
                       (list (car binding)
                             (transform-form (cadr binding)
                                             targets
                                             shadowed
                                             transform stop)))
                     (second form))
         ,@(mapcar #`,(transform-form x1 targets 
                                      updated-shadowed
                                      transform stop)
                   (cddr form)))))

  (defun transform-let*-form (form targets shadowed transform stop)
    (let ((updated-shadowed shadowed))
      `(let* ,(loop for binding in (cadr form)
                 collect (list (car binding)
                               (transform-form (cadr binding)
                                               targets
                                               updated-shadowed
                                               transform stop))
                 ;; Note: side effect on updated-shadowed here.
                 do (let ((name (symbol-name (car binding))))
                      (setf updated-shadowed
                            (union updated-shadowed
                                   (when (member name targets 
                                                 :test #'string-equal)
                                     (list name))))))
         ,@(mapcar #`,(transform-form x1 targets updated-shadowed
                                      transform stop)
                   (cddr form)))))

  (defun transform-form (form targets shadowed transform stop)
    "Given a form, find every occurence of symbols in TARGETS in
    the form, and replace them with (@ this props *) if they are not
    shadowed by other local bindings."
    ;; Considered local bindings are LET, LET*, LAMBDA and
    ;; lambda-alike.
    ;;
    ;; Note: This does not mean we encourage local bindings to shadow
    ;; targets.
    (cond ((and stop (funcall stop form)) form)
          ((atom form) 
           (if (and (symbolp form)
                    (member (symbol-name form) targets 
                            :test #'string-equal)
                    (not (member (symbol-name form) shadowed
                                 :test #'string-equal)))
               (funcall transform form)
               form))
          ((and (symbolp (car form))
                (string-equal (symbol-name (car form)) "LET"))
           (transform-let-form form targets shadowed transform stop))
          ((and (symbolp (car form))
                (string-equal (symbol-name (car form)) "LET*"))
           (transform-let*-form form targets shadowed transform stop))
          ((and (symbolp (car form))
                (string-equal (symbol-name (car form)) "LAMBDA"))
           (transform-lambda-form form targets shadowed transform stop))
          (t (mapcar #`,(transform-form x1 targets shadowed transform stop) 
                     form))))

  (defun attribute-transformer (attributes)
    (lambda (form shadowed)
      (transform-form form attributes shadowed
                      #`(@ this props ,x1)
                      nil)))

  (defun state-transformer (states)
    (lambda (form shadowed)
      (transform-form form states shadowed
                      #`(@ this state ,x1)
                      #`,(and (symbolp (car x1))
                              (string-equal "UPDATE-STATE"
                                            (symbol-name (car x1)))))))

  (defun compile-psx (expr attribute-names shadowed)
    ;; (labels ((try-replace-with-local-attribute (expr)
    ;;            (if (atom expr)
    ;;                (if (and (symbolp expr)
    ;;                         (member (symbol-name expr)
    ;;                                 attribute-names :test #'string-equal))
    ;;                    `(@ this props ,expr)
    ;;                    expr)
    ;;                (mapcar #'try-replace-with-local-attribute expr)))
    ;;          (transform-tag-attribute (attribute)
    ;;              (cond ((string-equal (car attribute) "style")
    ;;                     (destructuring-bind (name . values) attribute
    ;;                       `(,name (create ,@(mapcan 
    ;;                                          (lambda (value-pair)
    ;;                                            ;; TODO(breakds): make
    ;;                                            ;; sure style-name is a
    ;;                                            ;; keyword.
    ;;                                            (case (car value-pair) 
    ;;                                              (:z-index (list 'z-index 
    ;;                                                              (cadr value-pair)))
    ;;                                              (t value-pair)))
    ;;                                          ;; TODO(breakds): Check
    ;;                                          ;; whether value is pair
    ;;                                          ;; of 2.
    ;;                                          (group values 2))))))
    ;;                    (t (cons (car attribute)
    ;;                             (try-replace-with-local-attribute (cdr attribute)))))))
    (let ((trans-attr (attribute-transformer attribute-names)))
      (cond ((atom expr) (funcall trans-attr expr))
            ((keywordp (car expr))
             ;; Keyword case, can be either a standard html tag, or a custom tag.
             ;; Call React.DOM.tag-name when it is a standard html-tag, or the custom
             ;; ReactClass constructor otherwise.
             ;; TODO(breakds): Compile time error if tag is not recognizable.
             `(,(if (member (car expr) *html-tags*)
                    `(@ *react *dom* ,(unquantify-keyword (car expr)))
                    (unquantify-keyword (car expr)))
                ;; Handle input-attributes provided for this tag.
                ;; Note that we DO NOT allow for PSX syntax in
                ;; attributes.
                ;;
                ;; This is understandable because we never put html
                ;; code inside html attributes.
                (create ,@(mapcan #'transform-tag-attribute
                                  (cadr expr)))
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
                                   render (lambda () 
                                            ,',(compile-psx `(progn ,@body)
                                                            (mapcar #`,(symbol-name (car x1))
                                                                    attributes)))))))
       (setf (gethash (ps:ps-inline ,name) *realispic-symbol-table*)
             #',name))))
