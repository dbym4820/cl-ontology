(in-package :cl-user)
(defpackage cl-ontology-test-asd
  (:use :cl :asdf))
(in-package :cl-ontology-test-asd)

(defsystem cl-ontology-test
  :author "TomokiAburatani"
  :license "MIT License"
  :depends-on (:cl-ontology
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-ontology"))))
  :description "Test system for cl-ontology"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
