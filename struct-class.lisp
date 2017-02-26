;; learn the content from 《lisp 陈光喜》

(block abc
	   (print "helsdfk")
	   (return-from abc "exit here")
	   (print "qerqwer"))
(block nil
	   (print "helsdfk")
	   (return "exit here")
	   (print "qerqwer"))
(defstruct rectangle
  h
  w)

(defstruct mycircle
  r)
(defun area (x)
  (cond ((rectangle-p x)
	 (* pi (rectangle-h x) (rectangle-w x)))
	((mycircle-p x)
	 (* pi (expt (mycircle-r x) 2)))))
(defun example()
  (let ((r(make-rectangle))
	(s(make-mycircle)))
    (setf (rectangle-h r) 2
	  (mycircle-r s) 23
	  (rectangle-w r) 12)
    (list (area r) (area s))))
(defclass rec()
  (h w))
(defclass myc()
  (r))
(defmethod area1((x myc))
  (* pi (expt(slot-value x 'r) 2)))
(defmethod area1((x rec))
  (* (slot-value x 'h) (slot-value x 'w)))
(defun example1()
  (let ((r(make-instance 'rec))
	(s(make-instance 'myc)))
    (setf (slot-value r 'h) 2
	  (slot-value s 'r) 23
	  (slot-value r 'w) 12)
    (list (area1 r) (area1 s))))
(defclass stu ()
  (name
   sex
   number
   grade))
(make-instance 'stu)
;;结构体的成员变量（一般称之为 槽）的读取
(defstruct student
  name
  sex
  number)
(defparameter *a-stu* (make-student))
					; read slot-of student *a-stu*
(setf (student-name  *a-stu*) 'qintao)
(setf (student-sex  *a-stu*) 'female)
(setf (student-number  *a-stu*) 1002)
(student-name *a-stu*) 
(student-sex *a-stu*) 
(student-number *a-stu*)

(defclass piont()
  ((x :accessor piont-x)    ;使用 accessors 关键字 可以 便捷的访问 类的槽值
   (y :accessor piont-y)
   (z :accessor piont-z)))
(setf (piont-x *piont-a*) 12)
(setf (piont-y *piont-a*) 123)
(setf (piont-z *piont-a*) 345)      ;在访问类的槽值之前，必须要先给它赋值，否则访问会报错
;; add the value of piont's slot

(defun add-piont-value()
  (* (piont-x *piont-a*)
     (piont-y *piont-a*)
     (piont-z *piont-a*)))

(defun foo()
	   "the notes of function"
	   (print "hello,common lisp"))
