#|
変換後オントロジー検索用ユーティリティ群
|#
(in-package :cl-ontology)
(annot:enable-annot-syntax)

@export
(defmacro as-ontology (concept-name)
  (intern (string-upcase concept-name) :ontology.class))

@export
(defun get-children (concept-name)
  (let ((show-name
	  `(sub-class (eval (as-ontology ,concept-name)))))
    (eval show-name)))

@export
(defun get-id (concept-name)
  (let ((show-name
	  `(id (eval (as-ontology ,concept-name)))))
    (eval show-name)))

@export
(defun get-parents (concept-name)
  (let ((show-name
	  `(super-class (eval (as-ontology ,concept-name)))))
    (eval show-name)))

@export
(defun get-pos (concept-name)
  (let ((show-name
	  (list
	   `(pos-x (eval (as-ontology ,concept-name)))
	   `(pos-y (eval (as-ontology ,concept-name))))))
    (mapcar #'eval show-name)))
