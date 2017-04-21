(defun n!(n)
  (cond
    ((= n 1) 1)
    (t
     (* n (n! (1- n))))))  

 
(defun sum-linear-digit(endnumber step)  ;  自然数排列相加 （等加数列）
  (cond
    ((<= endnumber 0) 0)
    (t (+ endnumber (sum-linear-digit (- endnumber step) step))))) ;

(defun fibonacci(n)
  (if (or (= n 1)
	  (= n 0))
      1
      (+ (fibonacci (1- n))
	 (fibonacci (- n 2)))))

(defun fibonacci-a(n)
  (fib-swap 1 1 n))

(defun fib-swap(a b n)
  (if (or (= n 1)
	  (= n 0))
      a
      (fib-swap (+ a b) a (1- n))))

(defun hanoi(n)
  (transfer 'a 'b 'c n))

(defun transfer(from to temp n)
  (if (= n 1)                                  ;n=1   a->b
      (move-disk from to)
      (append (transfer from temp to (1- n))   ;n>=1 a->c a->b c->b
	      (move-disk from to )
	      (transfer temp to from (1- n)))))
  
(defun move-disk (from to)
   (list(list 'move 'disk 'from from 'to to)))


(defun palindromic-number(&optional number)   ;回文数判断
  (if number
	  (if (numberp number)
		  (setf number (format nil "~A" number)))
	  (progn
		(print "input a number:")
		(setf number (format nil "~A" (read)))))
  (let ((string number)
		(tag t))		   
	(do ((i 0 (1+ i))
		 (j (1- (length string)) (1- j)))
		((or (= i j)
			 (> i (1- j)))
		 tag)
	  (when (not(equalp (elt string i)
						(elt string j)))
		(setf tag nil)
		(return)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun threat (i j a b)                   ;;八皇后问题 
  (or
   (equal i a)
   (equal j b)
   (equal (- i j) (- a b))
   (equal (+ i j) (+ a b))))              ;对于两个皇后所在的位置是否相互威胁的判断
	 
(defun eight-queens-problem(queens-size)
  (queens-aux nil 0 queens-size))

(defun queens-aux (board n size)
  (cond
	((= n size)
	 (print (reverse board)))
	(t
	 (queens-sub board n 0 size))))
(defun queens-sub (board n m size)
  (cond
	(( = m size))
	(t
	 (cond
	  ((conflict n m board))    ;注意，该表达式，并非多余，很巧秒的地方
	   (t
		(queens-aux (cons (list n m)
						  board)
					(1+ n)
					size)))
	 (queens-sub board n (+ m 1) size))))
	 
(defun conflict (n m board)                           ;对于任意两个皇后是否冲突的判断，使用递归方法
  (cond
	(( null board) nil)
	((or
	  (threat n m (caar board) (cadar board))
	  (conflict n m (cdr board))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun searchb (start finish)
  (searcha (list start ) finish))
(defun searcha (queue finish)
  (cond
	((null queue) nil)
	((equal finish (car queue)) t)
	(t
	 (searcha (expand (car queue))
			  finish))))
(defun expand (path )
  (mapcar #'(lambda (children) (cons children path))
		  (get (car path) 'children)))
(defun depth(start finish)                           ;深度优先搜索
  (deptha (list(list start)) finish))
(defun deptha (queue finish)
  (cond
	((null queue) nil)
	((equal finish (caar queue))
	 (reverse (car queue)))
	(t
	 (deptha
	  (append(expand (car queue))
			 (cdr queue))
	  finish))))
(defun set-children()
  (setf (get 's 'children) '(l o)
		(get 'l 'children) '(m f)
		(get 'm 'children) '(n)
		(get 'n 'children) '(f)
		(get 'o 'children) '(p q)
		(get 'p 'children) '(f)
		(get 'q 'children) '(f)))
(defun breadth(start finish)                           ;广度优先搜索
  (breadth-a (list (list start )) finish))

(defun breadth-a (queue finish )                     
  (cond
	((null queue ) nil)
	((equal (caar queue)finish)
	 (reverse (car queue)))
	(t
	 (breadth-a (append (cdr queue)
					  (expand (car queue)))
			  finish))))
