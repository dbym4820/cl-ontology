(in-package :cl-user)
(defpackage cl-ontology
  (:use :cl)
  (:nicknames :ont))

(defpackage ontology.class
  (:use :cl))

(defpackage ontology.part
  (:use :cl))

(in-package :cl-ontology)
(annot:enable-annot-syntax)

#|
読み込むオントロジーのファイル
あとでインタラクティブに変更
|#
(defparameter *ontology-file* (concatenate 'string (namestring (asdf:component-pathname (asdf:find-system :cl-ontology))) "src/data/anime-ontology.xml"))


#|
オントロジーの概念を定義するクラス
|#
@export
(defclass ontology-class ()
  ((class-name :initarg :class-name :accessor class-name)
   (super-class :initarg :super-class :accessor super-class)
   (sub-class :initarg :sub-class :accessor sub-class)
   (id :initarg :id :accessor id)
   (pos-x :initarg :position-x :accessor pos-x)
   (pos-y :initarg :position-y :accessor pos-y)
   (part-concepts :initarg :part-concepts :accessor part-concepts)))


@export
(defclass part-concept ()
  ((role-name :initarg :role-name :accessor role-name)
   (id :initarg :id :accessor id)
   (role-holder :initarg :role-holder :accessor role-holder)
   (role-value :initarg :role-value :accessor role-value)
   (class-const :initarg :class-const :accessor class-const)
   (cardinality :initarg :cardinality :accessor cardinality)
   (kind :initarg :kind :accessor kind)
   (label :initarg :label :accessor label)
   (type :initarg :type :accessor type)
   (part-concepts :initarg :part-concepts :accessor part-concepts)
   (belonged-class :initarg :belonged-class :accessor belonged-class)))


@export
(defmacro defontology (concept-name contents)
  `(defparameter ,(intern (string-upcase concept-name) :ontology.class) ,contents))

@export
(defmacro defontology-part (concept-name contents)
  `(defparameter ,(intern (string-upcase concept-name) :ontology.part) ,contents))
