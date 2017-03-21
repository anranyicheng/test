;; 从群讨论中知道的排列组合方法，由于对于尾递归没有很好的理解，自己还不能写出独立的代码，保存以备后用
(defparameter *a* ())
(defparameter *b* '(1 2 3 4))
;;该算法根本就是一个错误的算法，没有明确的思路，
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
\


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
;; 下面的算法是有问题的，当随便输入一个小于等于1 的数，都会输出结果，逻辑也不是恨严谨
(defun cn (lst n)
  (let ((a (car lst))
	(b (cdr lst)))
    (if (> n 1)
	(append (mapcar (lambda (e) (cons a e))
			(cn b (1- n)))
		(if (consp b )      ; 这里的consp b ，简直就是拼凑出来的产物		
		    (cn b n) nil))
	(mapcar (lambda (e) (cons e nil))
		lst))))

;;(maplist (lambda (lst) (loop for n from 1 to 4 do (format t "(cn ~s ~a): ~a~%" lst n (cn lst n)))) '( a b c d))

(defun proper-subset(lst n)
  (cond
    ((or (= n 0)
	 (null lst)) '(()))
    ((= n (length lst)) (list lst))
    (t (let ((head (car lst))
	     (rest(cdr lst)))
	 (append (mapcar (lambda (x) (cons head x))
			 (proper-subset rest (1- n)))
		 (proper-subset rest n))))))

;; 运用proper—subset 完全可以构造出一个求任意长度列表的真子集，同时子集的问题在其基础之上也能够基本解决
(defun getproper-subset (tree)
  (let ((lst nil))
    (dotimes (i (1+ (length tree)))
      (setf lst (append
		 lst
		 (proper-subset tree i))))
    lst))     ;该函数可以将所有的真子集输出，在其基础之上（得到真子集）可以进行任意操作
;; 增加几个尾递归的例子
 (defun our-l(item lst)
	   (if (null lst) 
	       nil
	       (if (equal item (car lst))
		   lst
		   (our-l item (cdr lst)))))

;;;;;;;;;; test
 >(our-l 'a '(q w e r))
  0: (OUR-L A (Q W E R))
    1: (OUR-L A (W E R))
      2: (OUR-L A (E R))
        3: (OUR-L A (R))
          4: (OUR-L A NIL)
          4: OUR-L returned NIL
        3: OUR-L returned NIL
      2: OUR-L returned NIL
    1: OUR-L returned NIL
  0: OUR-L returned NIL
;;;;;;;;
(defun fib ( a1 a2 a3 )
	   (if (> a2 a3) 
	       a1
	       (fib (* a2 a1)
		    (1+ a2)
		    a3)))
;;;;;; test 
>(fib 1 1 4)
  0: (FIB 1 1 4)
    1: (FIB 1 2 4)
      2: (FIB 2 3 4)
        3: (FIB 6 4 4)
          4: (FIB 24 5 4) ;尾递归的函数，每一次求值都是最后的一步，当最后一次函数调用时，已经求得最终的值
          4: FIB returned 24
        3: FIB returned 24
      2: FIB returned 24
    1: FIB returned 24
  0: FIB returned 24
;;;;;;

;;一个非递归与之对比

(defun fi (n)
	   (if (= n 1)
	       1
	       (* n (fi (1- n)))))
;;test 
 0: (FI 5)
    1: (FI 4)
      2: (FI 3)
        3: (FI 2)
          4: (FI 1)    ;非尾递归的函数，必须先求出最后一次函数调用的结果，
                        ;然后再与前面的每一次的值相乘，才求出最后的结果，它要维护的链表很长，系统开销很大
          4: FI returned 1
        3: FI returned 2
      2: FI returned 6
    1: FI returned 24
  0: FI returned 120
