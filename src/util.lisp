#|
基本ユーティリティ
|#
(in-package :cl-ontology)

(defun flatten (ls)
  (cond ((null ls) nil)
        ((atom ls) (list ls))
        (t (append (flatten (car ls)) (flatten (cdr ls))))))
