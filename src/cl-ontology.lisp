(in-package :cl-user)
(defpackage cl-ontology
  (:use :cl)
  (:nicknames :ont))


(defpackage ontology.class
  (:use :cl))

(in-package :cl-ontology)


#|
読み込むオントロジーのファイル
あとでインタラクティブに変更
|#
(defparameter *ontology-file* (concatenate 'string (namestring (asdf:component-pathname (asdf:find-system :cl-ontology))) "src/data/anime-ontology.xml"))

