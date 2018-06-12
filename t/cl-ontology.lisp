(in-package :cl-user)
(defpackage cl-ontology-test
  (:use :cl
        :cl-ontology
        :prove))
(in-package :cl-ontology-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-ontology)' in your Lisp.

(setf *enable-colors* nil)
(defparameter *ontology-pathname*
  (merge-pathnames #P"t/ontology/anime-ontology.xml"
		   (asdf:system-source-directory 'cl-ontology)))

(plan 2)

(subtest "Ontology convert test"
  (ok (cl-ontology::convert-ontology :file-path *ontology-pathname* :update t)))

(subtest "basic-concept test"
  (let* ((test-any-instance (make-concept "Any" :c-type :basic-concept))
	 (test-new-instance (make-concept "New" :c-type :basic-concept :class-restriction test-any-instance)))
    (is (clon::concept-name test-any-instance) "Any")
    (is (clon::concept-name test-new-instance) "New")))


(finalize)
