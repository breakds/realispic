;;;; realispic-test.lisp
;;;; Author: BreakDS <breakds@gmail.com>


(in-package #:breakds.realispic)

(defsuite* (test-all :in root-suite
                     :documentation "Unit tests for realispic."))


(deftest match-symbol-test ()
  (is (match-symbol 'abc "abc"))
  (is (not (match-symbol :abc)))
  (is (match-symbol 'a)))



(deftest compile-matcher-test ()
  (is (equal (compile-matcher 'transform-let '((let-symbol :symbol "let") 
                                               bindings
                                               &rest body)
                              '(loop for binding in bindings
                                  collect (car binding)))
             '(transform-let (form shadowed)
               (destructuring-bind (let-symbol bindings &rest body) form
                (when (and (match-symbol let-symbol "let"))
                  (loop for binding in bindings
                     collect (car binding))))))))

(deftest transform-form-test-case (form targets transform stop expected)
  (is (equal expected
             (transform-form form targets nil transform stop))))

(deftest transform-form-test ()
  (transform-form-test-case '(+ 1 2) '("a" "b") 
                            #`(@ this props ,x1)
                            nil
                            '(+ 1 2))
  
  (transform-form-test-case '(+ b a) '("a" "b") 
                            #`(@ this props ,x1)
                            nil
                            '(+ (@ this props b) (@ this props a)))

  (transform-form-test-case 
   '(lambda (x y)
     (+ x y a b))
   '("x" "a")
   #`(@ this props ,x1)
   nil
   '(lambda (x y)
     (+ x y (@ this props a) b)))
  
  (transform-form-test-case
   '(let ((a 12)
          (b c))
     (+ a b c))
   '("a" "c")
   #`(@ this props ,x1)
   nil
   '(let ((a 12)
          (b (@ this props c)))
     (+ a b (@ this props c))))

  (transform-form-test-case
   '(let* ((c (+ a 2))
           (a 12)
           (b (+ a 2)))
     (+ a b))
   '("a" "b")
   #`(@ this props ,x1)
   nil
   '(let* ((c (+ (@ this props a) 2))
           (a 12)
           (b (+ a 2)))
     (+ a b)))

  (transform-form-test-case
   '(let ((a (lambda (x y)
               (+ x y c)))
          (b (lambda (x y z)
               (+ (funcall a x y) z))))
     (funcall b x y d))
   '("a" "b" "c" "x" "d")
   #`(@ this props ,x1)
   nil
   '(let ((a (lambda (x y)
               (+ x y (@ this props c))))
          (b (lambda (x y z)
               (+ (funcall (@ this props a) x y) z))))
     (funcall b (@ this props x) y (@ this props d))))
  (transform-form-test-case
   '(lambda (x y)
     (update-state a x b y))
   '("a b")
   #`(@ this state ,x1)
   #`,(and (symbolp (car x1))
           (string-equal "update-state"
                         (symbol-name (car x1))))
   '(lambda (x y)
     (update-state a x b y))))
  

      
     
     
     
  
      
                              
                              


    
