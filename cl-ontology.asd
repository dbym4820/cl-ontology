(in-package :cl-user)
(defpackage cl-ontology-asd
  (:use :cl :asdf))
(in-package :cl-ontology-asd)

(defsystem cl-ontology
  :version "0.1"
  :author "TomokiAburatani"
  :license "MIT License"
  :depends-on (:cl-ppcre
               :cxml
	       :cl-annot)
  :components ((:module "src"
                :components
                ((:file "cl-ontology"))))
  :description "Library to operate ontology written by hozo"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-ontology-test))))
