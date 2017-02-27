;; 从群讨论中知道的排列组合方法，由于对于尾递归没有很好的理解，自己还不能写出独立的代码，保存以备后用
(defparameter *a* ())
(defparameter *b* '(1 2 3 4))
(defun pailie(x)
  (let ((len (length x))
	(ls ()))
    (if (= len 0)
	nil
	(dolist (var x *a*)
	  (dotimes (i (length (remove  var x)))
	    (push  (nth i (remove var x)) ls))
	  (push (append (list var) ls) *a*)
	  (setf ls())

	  (do ((j (1- (length (remove var x))) (1- j)))
	      ((< j 0))
	    (push (nth j (remove var x)) ls))
	  (push (append (list var )ls) *a*)
	  (setf ls())))))      ;; 以上是我完成的，感觉差距不是一般的大啊   
(defun c2(lst)
	   (let ((a (car lst))
		 (b (cdr lst)))
	     (if (consp b)
		 (append (mapcar (lambda (c)(list a c)) b)
			 (c2 b))
		 nil)))

(defun c22(lst)
	   (let ((a (car lst))
		 (b (cdr lst)))
	     (if (consp b)
		 (append (mapcar (lambda (c)(cons a c)) b)
			 (c22 b))
		 nil)))
(defun cn (lst n)
  (let ((a (car lst))
	(b (cdr lst)))
    (if (> n 1)
	(append (mapcar (lambda (e) (cons a e))
			(cn b (1- n)))
		(if (consp b )
		    (cn b n) nil))
	(mapcar (lambda (e) (cons e nil))
		lst))))

;;(maplist (lambda (lst) (loop for n from 1 to 4 do (format t "(cn ~s ~a): ~a~%" lst n (cn lst n)))) '( a b c d))
(defun comb(lst n)
  (cond
    ((or (= n 0)
	 (null lst)) '(()))
    ((= n (length lst)) (list lst))
    (t (let ((head (car lst))
	     (rest(cdr lst)))
	 (append (mapcar (lambda (x) (cons head x))
			 (comb rest (1- n)))
		 (comb rest n))))))


;; 增加几个尾递归的例子
 (defun our-l(item lst)
	   (if (null lst) 
	       nil
	       (if (equal item (car lst))
		   lst
		   (our-l item (cdr lst)))))

(defun fib ( a1 a2 a3 )
	   (if (> a2 a3) 
	       a1
	       (fib (* a2 a1)
		    (1+ a2)
		    a3)))
