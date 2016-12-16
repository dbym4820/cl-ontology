(in-package :cl-user)
(defpackage cl-ontology
  (:use :cl))


(defpackage ontology.class
  (:use :cl))

(in-package :cl-ontology)
(annot:enable-annot-syntax)

#|
ユーティリティ
あとで別ファイルに分割
|#
(defun flatten (ls)
  (cond ((null ls) nil)
        ((atom ls) (list ls))
        (t (append (flatten (car ls)) (flatten (cdr ls))))))

#|
読み込むオントロジーのファイル
あとでインタラクティブに変更
|#
(defparameter *ontology-file* (concatenate 'string (namestring (asdf:component-pathname (asdf:find-system :cl-ontology))) "src/data/anime-ontology.xml"))

@export
(defun set-ontology-file (file-path)
  (setf *ontology-file* file-path))

#|
法造オントロジーをCLOSに変換するためのユーティリティ
|#
(defun xml-to-list (path-name)
  (cxml:parse-file path-name (cxml-xmls:make-xmls-builder)))


(defun find-list (origin-list key)
  (let ((ont-list nil))
    (labels ((finder (tree)
	       (cond
		 ((null tree) nil)
		 ((listp tree)
		  (if (stringp (car tree))
		      (if (string= (car tree) key)
			  (push tree ont-list)
			  (mapcar #'(lambda (lst) (if (listp lst) (finder lst))) tree))))
		 (t nil))))
      (finder origin-list))
    (reverse ont-list)))


(defun parent-child-alist ()
  (loop for x in (find-list (xml-to-list *ontology-file*) "ISA")
	collect (cons
		 (second (first (second x)))
		 (second (second (second x))))))

(defun get-concept-list ()
  (delete-duplicates
   (flatten 
    (parent-child-alist)) :test #'string=))
  
(defun get-parent-concept (concept)
  (cdr (assoc concept (parent-child-alist) :test #'string=)))

(defun get-child-concepts (concept)
 (let ((child-list nil))
   (loop for concept-list in (mapcar #'(lambda (lst)
					 (list (cdr lst) (car lst))) (parent-child-alist))
	 when (string= concept (car concept-list))
	   do (push (cadr concept-list) child-list))
   child-list))

(defun get-concept-id (concept)
  (loop for x in (find-list (xml-to-list *ontology-file*) "CONCEPT")
	do (if (string= concept (third (car (find-list x "LABEL"))))
	       (return (second (car (second x)))))))


(defun get-concept-pos-x (concept)
  (loop for x in (find-list (xml-to-list *ontology-file*) "CONCEPT")
	do (if (string= concept (third (car (find-list x "LABEL"))))
	       (return (parse-integer (cadadr (second (car (find-list x "POS")))))))))
  
(defun get-concept-pos-y (concept)
  (loop for x in (find-list (xml-to-list *ontology-file*) "CONCEPT")
	do (if (string= concept (third (car (find-list x "LABEL"))))
	       (return (parse-integer (cadar (second (car (find-list x "POS")))))))))
  

#|
オントロジーの概念を定義するクラス
|#
(defclass ontology ()
  ((class-name :initarg :class-name :accessor class-name)
   (super-class :initarg :super-class :accessor super-class)
   (sub-class :initarg :sub-class :accessor sub-class)
   (id :initarg :id :accessor id)
   (pos-x :initarg :position-x :accessor pos-x)
   (pos-y :initarg :position-y :accessor pos-y)
   (other :initarg :other :accessor other)))



@export
(defmacro defontology (concept-name contents)
  `(defparameter ,(intern (string-upcase concept-name) :ontology.class) ,contents))

@export
(defun convert-ontology (&optional (file-name nil))
  (if file-name
      (set-ontology-file file-name)
      (set-ontology-file (concatenate 'string (namestring (asdf:component-pathname (asdf:find-system :cl-ontology))) "src/data/anime-ontology.xml")))
  (defparameter *ontology-list*
    (mapcar (lambda (concept)
	      (eval `(defontology ,concept
			 ,(make-instance 'ontology
					 :class-name concept
					 :super-class nil
					 :sub-class (mapcar #'(lambda (lst) (intern (string-upcase lst) :ontology.class)) (get-child-concepts concept))
					 :id (get-concept-id concept)
					 :position-x (get-concept-pos-x concept)
					 :position-y (get-concept-pos-y concept)))))
	    (get-concept-list))))


@export
(defmacro ontology (concept-name)
  (intern (string-upcase concept-name) :ontology.class))




#|
オントロジー検索用ユーティリティ群
|#
@export
(defun get-children (concept-name)
  (let ((show-name
	  `(sub-class (eval (ontology ,concept-name)))))
    (eval show-name)))

@export
(defun get-id (concept-name)
  (let ((show-name
	  `(id (eval (ontology ,concept-name)))))
    (eval show-name)))

@export
(defun get-parents (concept-name)
  (let ((show-name
	  `(super-class (eval (ontology ,concept-name)))))
    (eval show-name)))

@export
(defun get-pos (concept-name)
  (let ((show-name
	  (list
	   `(pos-x (eval (ontology ,concept-name)))
	   `(pos-y (eval (ontology ,concept-name))))))
    (mapcar #'eval show-name)))



#|
オントロジーXMLのCLOSへのコンバート
|#
(convert-ontology)
