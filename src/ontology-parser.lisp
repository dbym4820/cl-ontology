#|
法造オントロジーをCLOSに変換するためのユーティリティ
|#
(in-package :cl-ontology)
(annot:enable-annot-syntax)


@export
(defun set-ontology-file (file-path)
  (setf *ontology-file* file-path))

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





#|
基本概念クラスに関する情報取得
|#
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
	       (return (second (assoc "id" (second x) :test #'string=))))))


(defun get-concept-pos-x (concept)
  (loop for x in (find-list (xml-to-list *ontology-file*) "CONCEPT")
	do (if (string= concept (third (car (find-list x "LABEL"))))
	       (return (parse-integer (cadadr (second (car (find-list x "POS")))))))))

(defun get-concept-pos-y (concept)
  (loop for x in (find-list (xml-to-list *ontology-file*) "CONCEPT")
	do (if (string= concept (third (car (find-list x "LABEL"))))
	       (return (parse-integer (cadar (second (car (find-list x "POS")))))))))
     



#|
部分・属性概念に関する情報取得
|#
(defun get-part-concepts (concept)
  (loop for x in (find-list (xml-to-list *ontology-file*) "CONCEPT")
	do (if (string= concept (third (car (find-list x "LABEL"))))
	       (return (find-list x "SLOTS")))))

(defun get-next-roles (tree)
  (let ((lst nil))
    (loop for x in (fourth (car tree))
	  do (if (listp x)
		 (if (stringp (car x))
		     (if (string= (car x) "SLOTS")
			 (push x lst)))))
    lst))

(defun get-role-concepts (concept)
  (let ((slots (get-part-concepts concept)))
    (loop for x in slots
	  collect (cons (second (assoc "kind" (second x) :test #'string=)) (second (assoc "role" (second x) :test #'string=))))))

(defun get-role-id (concept-name role-concept-name)
  (loop for x in (get-part-concepts concept-name)
	do (if (string=
		role-concept-name
		(second (assoc "role" (second x) :test #'string=)))
	       (return (cons role-concept-name (second (assoc "id" (second x) :test #'string=)))))))
	       
	       












#|
オントロジーの概念を定義するクラス
|#
(defclass ontology-class ()
  ((class-name :initarg :class-name :accessor class-name)
   (super-class :initarg :super-class :accessor super-class)
   (sub-class :initarg :sub-class :accessor sub-class)
   (id :initarg :id :accessor id)
   (pos-x :initarg :position-x :accessor pos-x)
   (pos-y :initarg :position-y :accessor pos-y)
   (part-concepts :initarg :part-concepts :accessor part-concepts)))

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
   (part-concepts :initarg :part-concepts :accessor part-concepts)))

@export
(defmacro defontology (concept-name contents)
  `(defparameter ,(intern (string-upcase concept-name) :ontology.class) ,contents))

@export
(defun convert-ontology (&optional (file-name nil))
  (if file-name
      (set-ontology-file file-name)
      (set-ontology-file (concatenate 'string (namestring (asdf:component-pathname (asdf:find-system :cl-ontology))) "src/data/anime-ontology.xml")))
  (defparameter *ontology-class-list*
    (mapcar (lambda (concept)
	      (eval `(defontology ,concept
			 ,(make-instance 'ontology-class
					 :class-name concept
					 :super-class nil
					 :sub-class (mapcar #'(lambda (lst) (intern (string-upcase lst) :ontology.class)) (get-child-concepts concept))
					 :id (get-concept-id concept)
					 :position-x (get-concept-pos-x concept)
					 :position-y (get-concept-pos-y concept)
					 :part-concepts (get-role-concepts concept)))))
	    (get-concept-list)))



 	;; (flatten (mapcar #'(lambda (lst) (mapcar #'cdr (get-role-concepts lst))) *ontology-list*))
  
  ;; (defparameter *ontology-part-list* nil)
  ;; (loop for concept in (get-concept-list)
  ;; 	do (loop for x in (get-role-concepts concept)
  ;; 		 do (push
  ;; 		     (eval `(defontology ,(cdr x)
  ;; 				,(make-instance 'part-concept
  ;; 						:role-name (cdr x)
  ;; 						:id (get-role-id (cdr x))
  ;; 						:role-holder ""
  ;; 						:role-value ""
  ;; 						:class-const ""
  ;; 						:cardinality ""
  ;; 						:kind ""
  ;; 						:label ""
  ;; 						:type ""
  ;; 						:part-concepts (get-role-concepts concept))))
  ;; 		     *ontology-part-list*))))
  )


#|
オントロジーXMLのCLOSへのコンバート
|#
(convert-ontology)
