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
(defun get-parts (concept)
  (loop for x in (find-list (xml-to-list *ontology-file*) "CONCEPT")
	do (if (string= concept (third (car (find-list x "LABEL"))))
	       (return (find-list x "SLOTS")))))


(defun analyze-ontology-part (base-concept &key attribute-name concept-number)
  (let* ((part-lst (loop for x in (remove-duplicates
				   (mapcar #'(lambda (lst)
					       (if (listp lst)
						   (if (stringp (car lst))
						       (if (string= (car lst) "SLOT")
							   lst))))
					   (cdr (car (get-parts base-concept)))))
			 when (not (null x))
			   collect x)))
    (if (null concept-number)
	(length part-lst)
	(loop for concept-list in part-lst
	      for count from 1
	      when (string= (car concept-list) "SLOT")
		do (if (= count concept-number)
		       (let ((attributes (second concept-list)))
			 (labels ((get-attr (attr)
				    (return (second (assoc attr attributes :test #'string=)))))
			   (cond
			     ((equal attribute-name :role-name) (get-attr "role"))
			     ((equal attribute-name :role-id) (get-attr "id"))
			     ((equal attribute-name :role-holder) (get-attr "rh_name"))
			     ((equal attribute-name :class-const) (get-attr "class_constraint"))
			     ((equal attribute-name :value-attr) (get-attr "value"))
			     ((equal attribute-name :cardinal) (get-attr "num"))
			     ((equal attribute-name :kind) (get-attr "kind"))
			     ((equal attribute-name :label) (get-attr "label"))))))))))
        
      

#|
オントロジーコンバータ
|#
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
					 :part-concepts nil))))
	    (get-concept-list)))

  (defparameter *ontology-parts-list*
    (mapcar (lambda (concept)
	      (loop for lst-count below (1+ (analyze-ontology-part concept))
		    when (not (null (analyze-ontology-part concept
							   :attribute-name :role-name
							   :concept-number lst-count)))     
		      collect (eval `(defontology-part
					 ,(concatenate 'string
						       concept
						       "-"
						       (analyze-ontology-part concept
									      :attribute-name :role-name
									      :concept-number lst-count))
					 ,(labels ((part-analyze (attr)
						     (analyze-ontology-part concept
									    :attribute-name attr
									    :concept-number lst-count)))
					    (eval
					     (make-instance 'part-concept
							    :role-name (part-analyze :role-name)
							    :id (part-analyze :role-id)
							    :role-holder (part-analyze :role-holder)
							    :role-value (part-analyze :value-attr)
							    :class-const (part-analyze :class-const)
							    :cardinality (part-analyze :cardinal)
							    :kind (part-analyze :kind)
							    :label (part-analyze :label)
							    :belonged-class (intern concept :ontology.class))))))))
			      (get-concept-list))))
						

#|
オントロジーXMLのCLOSへのコンバート
|#
(convert-ontology)
