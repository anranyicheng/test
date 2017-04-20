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
