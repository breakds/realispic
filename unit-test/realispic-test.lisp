;;;; realispic-test.lisp
;;;; Author: BreakDS <breakds@gmail.com>


(in-package #:breakds.realispic)

(defsuite* (test-all :in root-suite
                     :documentation "Unit tests for realispic."))


(deftest match-symbol-test ()
  (is (match-symbol 'abc "abc"))
  (is (not (match-symbol :abc)))
  (is (match-symbol 'a)))

(deftest compile-psx-simple-test ()
  ;; Single matcher cases
  
  ;; 1. Attributes
  (is (equal (compile-psx 'a :attribute-names '("a"))
	     '(render (lambda () (@ this props a)))))
  
  ;; 2. Let/Let*
  (is (equal (compile-psx '(let ((a 12)) a)
			  :attribute-names '("a"))
	     '(render (lambda () (let ((a 12)) a)))))

  (is (equal (compile-psx '(let ((a b)) (+ a b))
			  :attribute-names '("a" "b"))
	     '(render (lambda () 
			(let ((a (@ this props b)))
			  (+ a (@ this props b)))))))

  (is (equal (compile-psx '(let* ((a b) (b a)) (+ a b))
			  :attribute-names '("a" "b"))
	     '(render (lambda () 
			(let* ((a (@ this props b))
			       (b a))
			  (+ a b))))))

  (is (equal (compile-psx '(let ((a 12)) a))
	     '(render (lambda () (let ((a 12)) a)))))

  ;; 3. State references
  (is (equal (compile-psx '(let ((a (:state a))) (+ a (:state b)))
			  :state-defs '((a 1) (b 2)))
	     '(render (lambda ()
			(let ((a (@ this state a)))
			  (+ a (@ this state b)))))))

  ;; 4. Lambda
  (is (equal (compile-psx '(lambda (a) (+ a b))
			  :attribute-names '("a" "b"))
	     '(render (lambda ()
			(lambda (a)
			  (+ a (@ this props b)))))))

  ;; 5. PSX Tags
  (is (equal (compile-psx '(:div ((width (:state a))
				  (height b))
			    "div content")
			  :state-defs '((a 10)))
	     '(render (lambda ()
			((@ *react *dom* div) (create width (@ this state a)
						      height b)
			 "div content")))))

  (is (equal (compile-psx '(:customized ((width (:state a))
					 (height b))
			    "content")
			  :state-defs '((a 10)))
	     '(render (lambda ()
			(customized (create width (@ this state a)
					    height b)
				    "content")))))

  ;; 6. Top level labels
  (is (equal (compile-psx '(labels ((func-1 (x) (+ 1 x))
				    (func-2 () nil))
			    (:a ((href "link")) x))
			  :attribute-names '("x"))
	     '(render (lambda ()
			((@ *react *dom* a) (create href "link") (@ this props x)))
	       func-1 (lambda (x) (+ 1 x))
	       func-2 (lambda () nil)))))


(deftest compile-psx-complicated-test ()
  ;; Simple Test
  (is (equal (compile-psx '(+ 1 1))
	     '(render (lambda () (+ 1 1)))))
  
  ;; Complicated Test
  (is (equal (compile-psx '(labels ((toggle-full-summary ()
				     (update-state full-summary
						   (not (:state full-summary))))
				    (switch-jewel-plan (id)
				     (update-state jewel-plan-id (+ fixed-id id))))
			    (:div ((class "panel panel-success"))
			     (:div ((class "panel-heading"))
			      (@ armor-set defense))
			     (:table ((class "table"))
			      (:tr ()
				   (:th ((class "col-md-2")
					 (style :text-align "center"))
					"Current ID")
				   (:th ((class "col-md-10")
					 (style :text-align "left"
                                                :color summary-color))
					"Summary"))
			      (:line ((armor-set armor-set)
				      (summary (:state full-summary))
				      (id (:state jewel-plan-id)))))))
			  :state-defs '((full-summary "")
					(jewel-plan-id 0))
			  :attribute-names '("armor-set" "fixed-id" "summary-color"))
	     '(render (lambda ()
			((@ *react *dom* div) (create class-name "panel panel-success")
			 ((@ *react *dom* div) (create class-name "panel-heading")
			  (@ (@ this props armor-set) defense))
			 ((@ *react *dom* table) (create class-name "table")
			  ((@ *react *dom* tr) (create) 
			   ((@ *react *dom* th) (create class-name "col-md-2"
							style (create :text-align "center"))
			    "Current ID")
			   ((@ *react *dom* th) (create class-name "col-md-10"
							style (create :text-align "left"
                                                                      :color (@ this 
                                                                                props 
                                                                                summary-color)))
			    "Summary"))
			  (line (create armor-set (@ this props armor-set)
					summary (@ this state full-summary)
					id (@ this state jewel-plan-id))))))
	       toggle-full-summary (lambda ()
                                     ((@ this set-state)
                                      (create full-summary 
                                              (not (@ this state full-summary)))))
	       switch-jewel-plan (lambda (id)
				   ((@ this set-state)
                                    (create jewel-plan-id (+ (@ this props fixed-id) id)))))))

  ;; psx-only
  (is (equal (compile-psx '(:customized-tag () "haha") :psx-only t)
	     '(customized-tag (create) "haha"))))

(deftest single-dependency-test (form expected-dependencies)
  (multiple-value-bind (compiled dependencies)
      (compile-psx form :psx-only t)
    ;; Set equality test
    (is (= (length expected-dependencies) 
	   (length dependencies)
	   (length (intersection expected-dependencies
				 dependencies
				 :test #'string-equal))))))
    

(deftest compile-pxs-dependency-test ()
  (single-dependency-test '(:tag1 () 12) '("tag1"))
  (single-dependency-test '(:div () (:fun () (+ x y) (:fun-stuff () "fun")))
			  '("fun-stuff" "fun")))

			    








