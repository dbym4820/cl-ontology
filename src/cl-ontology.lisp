(in-package :cl-user)
(defpackage cl-ontology
  (:use :cl)
  (:nicknames :clon)
  (:import-from :alexandria
                :read-file-into-string)
  (:export :convert-ontology
           :make-concept
           :add-concept
           :append-concept
           :clear-ontology
           :show-concepts
           :find-concept
           :find-attribute
           :show-attribute
	   :concept-inherit-p))
(in-package :cl-ontology)

#|
オントロジーに表れる概念クラスの定義
|#
(defclass concept ()
  ((concept-name :initform "any" :initarg :name :accessor concept-name)))

(defclass basic-concept (concept)
  ((property-list :initform nil :initarg :property :accessor property-list)
   (parent-concept :initform nil :initarg :parent-concept :accessor parent-concept)))

(defclass non-basic-concept (concept)
  ((role-name :initform "new" :initarg :name :accessor concept-name)
   (class-restriction :initform nil :initarg :class-restriction :accessor class-restriction)
   (role-holder :initform "new" :initarg :role-holder :accessor role-holder)
   (cardinality :initform 1 :initarg :cardinality :accessor cardinality)
   (concept-type :initform :part-of :initarg :concept-type :reader concept-type)))

(defclass attribute-concept (non-basic-concept)
  ((concept-type :initform :attribute-of)))

(defclass part-of-concept (non-basic-concept)
  ((concept-type :initform :part-of)))

(defclass relation-concept (concept)
  ((relation-name :initform "new" :initarg :name :accessor concept-name)
   (part-concept-list :initform "new" :initarg :part :accessor part-list)))

(defclass relation-part (part-of-concept)
  ((concept-type :initform :relation-part-of)))

(defclass ontology ()
  ((theme :initform "any" :initarg :ontology-theme :reader theme)
   (concept-list :initform `(,(make-instance 'basic-concept :name "whole-root")) :accessor concept-list)))

#|
オントロジー定義の操作に関するメソッド群
|#
;;; 概念定義
(defun make-concept (concept-name &key (c-type :basic-concept) (class-restriction (make-instance 'basic-concept)) (cardinality 1) (rh-name ""))
  (cond ((eql c-type :basic-concept)
         (make-instance 'basic-concept :name concept-name))
        ((eql c-type :attribute-concept)
         (make-instance 'attribute-concept :name concept-name :class-restriction class-restriction :cardinality cardinality :role-holder rh-name))
        ((eql c-type :part-of-concept)
         (make-instance 'part-of-concept :name concept-name :class-restriction class-restriction))
        (t 
         (make-instance 'basic-concept :name concept-name))))
  
(defgeneric append-concept (concept ontology))

;;; オントロジーに基本概念を挿入
(defmethod append-concept ((new-class basic-concept) (target-ontology ontology))
  (setf (concept-list target-ontology)
        (append
          (if (consp (concept-list target-ontology))
                   (concept-list target-ontology)
                        `(,(concept-list target-ontology)))
               (cons new-class nil))))

;;; オントロジーに関係概念を挿入
(defmethod append-concept ((new-relation relation-part) (target-ontology ontology))
  (setf (concept-list target-ontology)
        (append
          (if (consp (concept-list target-ontology))
                   (concept-list target-ontology)
                        `(,(concept-list target-ontology)))
           (cons new-relation nil))))

;;; 基本概念にプロパティを挿入
(defmethod append-concept ((new-property non-basic-concept) (target-concept basic-concept))
  (setf (property-list target-concept)
        (append
          (if (consp (property-list target-concept))
                   (property-list target-concept)
                        `(,(property-list target-concept)))
           (cons new-property nil))))

;;; 関係概念にプロパティを挿入
(defmethod append-concept ((new-property part-of-concept) (target-relation relation-concept))
  (setf (property-list target-relation)
        (append
          (if (consp (property-list target-relation))
                   (property-list target-relation)
                        `(,(property-list target-relation)))
           (cons new-property nil))))

;;; オントロジーの中身をクリア
(defgeneric clear-ontology (ontology)
  (:method (ontology)
    (setf (concept-list ontology) `(,(make-instance 'basic-concept :name "whole-root")))))


#|
デフォルトのオントロジーセット
|#
;;; デフォルトのオントロジー
(defparameter *default-ontology* (make-instance 'ontology))

;;; オントロジー取得元のファイル
(defparameter *default-ontology-file*
  (concatenate 'string
                      (namestring (asdf:system-source-directory 'cl-ontology)) "src/ontology/anime-ontology.xml"))


#|
法造オントロジーのXMLへのコンバート
|#

;;; オントロジーファイルをXMLリストに変換
(defun convert-ontology-xml (file-path)
  (setf *default-ontology-file* file-path)
  (xmls:parse (read-file-into-string file-path) :compress-whitespace t))

;;; ファイル名の取得
(defun get-expected-file-name-tags (&optional (xml-file-path *default-ontology-file*))
  (second (second (second (convert-ontology-xml xml-file-path)))))

;;; オントロジーIDの取得
(defun get-ont-id-tag (&optional (xml-file-path *default-ontology-file*))
  (second (first (second (convert-ontology-xml xml-file-path)))))

;;; 概念定義本体の取得
(defun get-w-concept-tags (&optional (xml-file-path *default-ontology-file*))
  (cdr (remove-if #'null (fourth (convert-ontology-xml xml-file-path)))))

;;; リストが示すXMLタグ名を取得
(defun get-tag-name (tag-list)
  (first tag-list))

;;; ConceptXMLのリストが指定したタグのものかを判別
(defun tag-p (tag-string target-list)
  (when (and (listp target-list) (stringp (car target-list)))
    (when (and (string= tag-string (get-tag-name target-list)))
      t)))

;;; Conceptを表す塊のリストを取得
(defun get-concept-tags (&optional (xml-file-path *default-ontology-file*))
  (remove-if #'null
	     (mapcar #'(lambda (tag-list)
			 (when (string= (get-tag-name tag-list) "CONCEPT")
			   (cdr tag-list)))
		     (get-w-concept-tags xml-file-path))))

;;; 各Conceptのラベル（名前）リストを取得
(defun get-concept-label (&optional (xml-file-path *default-ontology-file*))
  (mapcar #'(lambda (concept-tag-list)
                    (second (let ((concept-info
                                          (mapcar #'(lambda (tag-list)
                                                        (when (tag-p "LABEL" tag-list)
                                                              tag-list))
                                                        concept-tag-list)))
                              (remove-if #'null (second concept-info)))))
            (get-concept-tags xml-file-path)))

;;; 概念定義の先頭からの出現番号
(defun get-concept-position-from-ahead (concept-name &optional (xml-file-path *default-ontology-file*))
  (position concept-name (get-concept-label xml-file-path) :test #'string=))

;;; 特定概念の塊リストを取り出す
(defun get-specific-concept-tags (concept-name &optional (xml-file-path *default-ontology-file*))
  (remove-if #'null
	     (nth (get-concept-position-from-ahead concept-name xml-file-path) (get-concept-tags xml-file-path))))

;;; 部分/属性概念のリストを取得
(defun get-concept-slot-tags (concept-name &optional (xml-file-path *default-ontology-file*))
  (remove-if #'null
	     (mapcar #'(lambda (slot)
			 (when (tag-p "SLOT" slot)
			   slot))
		     (find-if #'(lambda (concept-tag)
				  (when (tag-p "SLOTS" concept-tag) t))
			      (get-specific-concept-tags concept-name xml-file-path)))))

;;; 指定基本概念のスロット内の1属性を抽出
(defun get-attribute-from-slot-tags (concept-name attribute &optional (xml-file-path *default-ontology-file*))
  (let ((concept-tags (get-concept-slot-tags concept-name xml-file-path)))
    (labels ((tmp-func (att)
               (find-if #'(lambda (attr)
                            (when (tag-p attribute attr)
                              t))
                        (second att))))
      (mapcar #'(lambda (slot-tag)
                  (tmp-func slot-tag))
              concept-tags))))

;;; 指定された基本概念が持つスロット情報を取得
(defun get-slot-tags (concept-name &optional (xml-file-path *default-ontology-file*))
  (mapcar #'list
          (get-attribute-from-slot-tags concept-name "role" xml-file-path)
          (get-attribute-from-slot-tags concept-name "kind" xml-file-path)
          (get-attribute-from-slot-tags concept-name "class_constraint" xml-file-path)
          (get-attribute-from-slot-tags concept-name "rh_name" xml-file-path)
          (get-attribute-from-slot-tags concept-name "num" xml-file-path)))

;;; XMLリストから基本概念の親子関係を取得
(defun get-child-parent (&optional (xml-file-path *default-ontology-file*))
  (mapcar #'(lambda (child-parent-id-list)
	      (let ((child-concept-label (second (first (first child-parent-id-list))))
		    (parent-concept-label (second (second (first child-parent-id-list)))))
	        (cons child-concept-label parent-concept-label)))
	  (remove-if #'null
		     (mapcar #'(lambda (tag-list)
				 (when (string= (get-tag-name tag-list) "ISA")
				   (cdr tag-list)))
			     (get-w-concept-tags xml-file-path)))))

;;; 親概念名を基に子概念を取得
(defun get-child-concept (concept-name &optional (xml-file-path *default-ontology-file*))
  (remove-if #'null
	     (mapcar #'(lambda (child-parent)
			 (when (string= (cdr child-parent) concept-name)
			   (car child-parent)))
	     (get-child-parent xml-file-path))))

;;; 子概念名を基に親概念を取得
(defun get-parent-concept (concept-name &optional (xml-file-path *default-ontology-file*))
  (remove-if #'null
	     (mapcar #'(lambda (child-parent)
			 (when (string= (car child-parent) concept-name)
			   (cdr child-parent)))
	     (get-child-parent xml-file-path))))

#|
オントロジーのCLOSへのコンバート
|#
(defun convert-ontology (&key (file-path *default-ontology-file*) (ont *default-ontology*) (update t))
  (if update
      (progn
        (clear-ontology ont)
        (convert-basic-concept file-path ont)
        (convert-isa-relation file-path ont)
        (convert-part-attribute-concept file-path ont)
        (show-concepts ont))
      file-path))

;;; 基本概念の変換
(defun convert-basic-concept (&optional (xml-file-path *default-ontology-file*) (ont *default-ontology*))
  (let ((c-list (get-concept-label xml-file-path)))
    (loop for c in c-list
            do (add-concept c ont :concept-type :basic-concept)
            finally (return
                          (format nil "~A" (show-concepts ont))))))

;;; is-a関係の変換
(defun convert-isa-relation (&optional (xml-file-path *default-ontology-file*) (ont *default-ontology*))
  ;; ここで，Basic-conceptのparent-conceptの中に親概念を突っ込む
  )

;;; 部分/属性概念の変換
(defun convert-part-attribute-concept (&optional (xml-file-path *default-ontology-file*) (ont *default-ontology*))
  (let ((c-list (get-concept-label xml-file-path)))
    (loop for c in c-list ;; c mean anime title string
          do (mapcar #'(lambda (slot)
                         (remove-if #'null
				    (let ((role-name (princ-to-string (second (assoc "role" slot :test #'string=))))
					  (class-const (princ-to-string (second (assoc "class_constraint" slot :test #'string=))))
					  (rh-name (princ-to-string (second (assoc "rh_name" slot :test #'string=))))
					  (cardinality (princ-to-string (second (assoc "num" slot :test #'string=)))))
				      (append-concept
				       (make-concept role-name :c-type :part-of-concept
				      			       :class-restriction class-const
				      			       :cardinality cardinality
				      			       :rh-name rh-name)
				       (find-concept c ont)))))
                         (get-slot-tags c))
          finally (format nil "~A" (show-concepts ont)))))

#|
CLOSオントロジー操作用API
|#

;;; オントロジーの中身表示
(defun show-ontology (&optional (ont *default-ontology*))
  (concept-list ont))

;;; オントロジーの中身（ラベル）表示
(defun show-concepts (&optional (ont *default-ontology*))
  (mapcar #'concept-name (show-ontology ont)))

;;; 基本概念の追加
(defun add-concept (concept-name &optional (ont *default-ontology*) &key (concept-type :basic-concept))
  (declare (ignorable concept-type))
  (append-concept (make-concept concept-name) ont))

;;; オントロジーの検索
(defun find-concept (concept-name &optional (ont *default-ontology*))
  (find-if #'(lambda (c)
                      (when (string= concept-name (concept-name c)) t))
              (show-ontology ont)))

;;; 基本概念の属性検索
(defun find-attribute ())

;;; CLOSオントロジーの各パラメータを文字列として表示
;;; あとでCLOSメソッドに変更
(defun show-attribute (attribute concept &optional (ont *default-ontology*))
  (let ((return-value
          (cond ((eql attribute :proper)
                 (property-list concept))
                ((eql attribute :role-name)
                 (concept-name concept))
                ((eql attribute :class-restriction)
                 (class-restriction concept))
                ((eql attribute :cardinality)
                 (cardinality concept))
                ((eql attribute :concept-type)
                 (concept-type concept))
		((eql attribute :parent)
		 ;; ここをクラス内の親概念取得メソッドに書き換え
		 (first (get-parent-concept (concept-name concept))))
		((eql attribute :child)
		 ;; ここをクラス内の親概念取得メソッドに書き換え
		 (get-child-concept (concept-name concept)))
                (t
                 nil))))
    (cond ((listp return-value)
           (remove-if #'null return-value))
          (t
           return-value))))

;;; 第１引数として与えた基本概念が第２引数として与えた基本概念の下位概念かを調べる述語（継承関係の有無を調べる）
;;; 後にCLOSクラスを引数として取るメソッドに変更
(defgeneric concept-inherit-p (source-concept target-concept &key))
(defmethod concept-inherit-p ((source-concept basic-concept) (target-concept basic-concept) &key (ont *default-ontology*) )
      (labels ((rec-pred (sou)
		 (cond ((null sou) nil)
		       ((string= (concept-name sou) (concept-name target-concept)) t)
		       (t (rec-pred (find-concept (show-attribute :parent sou)))))))
	(rec-pred source-concept)))
