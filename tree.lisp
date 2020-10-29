(defparameter *m* 34359738368)
(defparameter *a* 343597383)
(defparameter *c* 1)
(defparameter *tree* nil)
(defparameter *random-list* nil)
;; test values for populating the tree.
(defun clm-list (X0 m a c list-length)
  (let ((xn X0))
    (loop for x from 0 to list-length collect (setf xn (clm xn m a c)))))
(defun clm (Xn m a c)
  (if (and
       (> m 0)
       (>= a 0)
       (> m a)
       (>= c 0)
       (> m c)
       (>= Xn 0)
       (> m Xn))
      (mod (+(* a Xn) c) m)))

;;binary tree.
(defun make-tree (root left right)
  (list root left right))
(defun add (x tree)
  (cond ((null tree) (make-tree x nil nil))
	((= x (root tree)) tree)
	((< x (root tree))
	 (make-tree (root tree) (add x (left-branch tree)) (right-branch tree)))
	((> x (root tree))
	 (make-tree (root tree)(left-branch tree) (add x (right-branch tree))))))


(defun root (tree)
  (car tree))
(defun left-branch (tree)
  (cadr tree)
(defun right-branch (tree)
  (caddr tree))

#|
(node left right) so
node -> (car tree)
left_branch -> (cadr tree)
right_branch is -> (caddr tree)
|#
