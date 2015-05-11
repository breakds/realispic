;;;; compiler.lisp
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.realispic)


;; TODO(breakds): compiler utils should live in antoher file,
;; e.g. compiler-utils.lisp.
(eval-when (:compile-toplevel :load-toplevel :execute)
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
    `(,name (form shadowed)
            ;; Destructring bind the input form into the pattern.
            ,(if pattern
                 `(destructuring-bind 
                        ,(loop for entry in pattern
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
                                                    pattern))))
                      form
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
                                                     (t (error "compile-matcher: Too many specifiers in ~a." 
                                                               entry))))
                                                  ((eq (second entry) :keyword)
                                                   (if (= (length entry) 2)
                                                       `(keywordp ,(car entry))
                                                       (error "compile-matcher: Too many specifiers in ~a."
                                                              entry)))
                                                  (t (error "compile-matcher: Invalid entry ~a." entry)))))
                      ,@body))
                 `(when (atom form)
                    ,@body)))))

(defmacro def-code-walker (name args (&rest matchers) &body body)
  (with-gensyms (form shadowed enabled-matchers matcher result)
    `(defun ,name (form &key ,@args)
       (labels ((execute (,form ,shadowed &rest ,enabled-matchers) 
                  (or (loop 
                         for ,matcher in ,enabled-matchers
                         for ,result = (funcall ,matcher ,form ,shadowed)
                         when ,result
                         return ,result)
                      ,form))
                (initialize (&rest ,enabled-matchers)
                  (apply #'execute form nil ,enabled-matchers))
                ,@(mapcar #`,(compile-matcher (car x1)
                                              (second x1)
                                              (cddr x1))
                          matchers))
         
         ,@body))))

(defun one-of-symbols-p (input-symbol symbol-names)
  (member (symbol-name input-symbol) symbol-names
          :test #'string-equal))



(def-code-walker compile-psx (attributes states)
    ((atom-attribute () (when (and (one-of-symbols-p form attributes)
                                   (not (one-of-symbols-p form shadowed)))
                          `(@ this props ,form)))
     (atom-default () form))
  (initialize #'atom-attribute #'atom-default))
                                 
                                 
                     


                     
